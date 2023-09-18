module Test.Tasty.Ingredients.Spectrum
    (
        testSpectrum
    )
 where

import Test.Tasty
import Test.Tasty.Ingredients

import Data.Proxy
import Data.Typeable
import Test.Tasty.Options

import Trace.Hpc.Reflect ( clearTix, examineTix )
import Trace.Hpc.Mix ( readMix, Mix(..), MixEntry )
import Trace.Hpc.Tix ( TixModule(..), Tix(Tix) )

import Control.Concurrent.STM ( atomically, readTVar, retry, TVar )
import qualified Test.Tasty.Runners as TR
import qualified Data.IntMap as IntMap

import Control.Monad



newtype GetTestSpectrum = GetTestSpectrum Bool
  deriving (Eq, Ord, Typeable)

instance IsOption GetTestSpectrum where
    defaultValue = GetTestSpectrum False
    parseValue = fmap GetTestSpectrum . safeReadBool
    optionName = return "get-spectrum"
    optionHelp = return "Runs the test and produces a spectrum"
    optionCLParser = flagCLParser Nothing (GetTestSpectrum True)

data SpectrumResult = SpecRes {
          tix_module :: [TixModule],
          test_result :: Bool,
          test_name :: String
        }


{-
                    test status (t_res). A string? Leave it bool for now.
                       ↓
                   | pass ratio?  | e1 | e2 <- mix_file location
 test_name <-   t1 |   y (1/1)    | 1  | 27 <- number of evals
                t2 |   y          | 0  | 17
                t3 |   n (0/1)    | 17 | 5
                t4 |   n          | 5  | 0
                q1 |   n (4/5)    | 7  | 5
                        ↑
                      from amount of tests, if < 1 then fail

think about data format..?

-}


testSpectrum :: Ingredient
testSpectrum = TestManager [Option (Proxy :: Proxy GetTestSpectrum)] $
  \opts tree ->
    case lookupOption opts of
      GetTestSpectrum False -> Nothing
      GetTestSpectrum True -> Just $ do
         let ts = unfoldTastyTests tree
         print $ length ts
         r <- forM ts $ \(t_name,test) -> do
            clearTix
            t_res <- checkTastyTree 5000 test
            print t_res
            Tix res <- examineTix
            w_mixes <- addMixes res
            mapM_ (\(_,_,r) -> print (map snd $ filter (\(i,_) -> i /= 0) r)) w_mixes
            return (SpecRes res t_res t_name)
         return $ all test_result r


addMixes :: [TixModule] -> IO [(TixModule, Mix, [(Integer, MixEntry)])]
addMixes tixes = do
     mixes <- mapM (readMix [".hpc"] . Right) tixes
     let f t@(TixModule _ _ _ tx) m@(Mix _ _ _ _ mes) = (t,m, zip tx mes)
     return (zipWith f tixes mixes)

unfoldTastyTests :: TestTree -> [(String, TestTree)]
unfoldTastyTests = TR.foldTestTree (TR.trivialFold {TR.foldSingle = fs'}) mempty
  where
    fs' opts name test = [(name, TR.PlusTestOptions (opts <>) $ TR.SingleTest name test)]


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