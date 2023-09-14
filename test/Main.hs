{-# OPTIONS_GHC -fhpc #-}
{-# LANGUAGE TypeApplications #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Test.Tasty.Ingredients

import Data.List
import Trace.Hpc.Reflect ( clearTix, examineTix )
import Trace.Hpc.Mix ( readMix, Mix(..), MixEntry )
import Trace.Hpc.Tix ( TixModule(..), Tix(Tix) )

import Control.Concurrent.STM ( atomically, readTVar, retry, TVar )
import qualified Test.Tasty.Runners as TR
import qualified Data.IntMap as IntMap

import Control.Monad


main :: IO ()
main = do
    let ts = unfoldTastyTests tests
    print $ length ts
    forM_ ts $ \t -> do
      clearTix
      t_res <- checkTastyTree 5000 t
      print t_res
      Tix res <- examineTix
      w_mixes <- addMixes res
      mapM_ (\(_,_,r) -> print (map snd $ filter (\(i,_) -> i /= 0) r)) w_mixes
    return ()

addMixes :: [TixModule] -> IO [(TixModule, Mix, [(Integer, MixEntry)])]
addMixes tixes = do
     mixes <- mapM (readMix [".hpc"] . Right) tixes
     let f t@(TixModule _ _ _ tx) m@(Mix _ _ _ _ mes) = (t,m, zip tx mes)
     return (zipWith f tixes mixes)

unfoldTastyTests :: TestTree -> [TestTree]
unfoldTastyTests = TR.foldTestTree (TR.trivialFold {TR.foldSingle = fs'}) mempty
  where
    fs' opts name test = [TR.PlusTestOptions (opts <>) $ TR.SingleTest name test]

-- Tasty helpers
checkTastyTree :: Int -> TestTree -> IO Bool
checkTastyTree timeout test =
  case tryIngredients [TestReporter [] (\_ _ -> Just reportFun)] mempty with_timeout of
    Just act -> act
    _ -> return False
  where
    with_timeout = localOption (mkTimeout (fromIntegral timeout)) test
    waitUntilDone :: TVar TR.Status -> IO Bool
    waitUntilDone status_var = atomically $ do
      status <- readTVar status_var
      case status of
        TR.Done res -> return $ TR.resultSuccessful res
        _ -> retry

    reportFun :: TR.StatusMap -> IO (TR.Time -> IO Bool)
    -- We completely ignore the parallelism here
    reportFun smap = do
      results <- mapM waitUntilDone $ IntMap.elems smap
      return (\_ -> return $ and results)


tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]


qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^(7 :: Integer) - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1 :: Integer, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1 :: Integer, 2, 3] `compare` [1,2,2] @?= LT
  ]
