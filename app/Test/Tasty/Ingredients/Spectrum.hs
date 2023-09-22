{-# LANGUAGE RecordWildCards #-}
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
import Trace.Hpc.Tix ( TixModule(..), Tix(Tix), tixModuleName, tixModuleTixs )

import Control.Concurrent.STM ( atomically, readTVar, retry, TVar )
import qualified Test.Tasty.Runners as TR
import qualified Data.IntMap as IntMap

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Control.Monad
import Data.List (intercalate)
import Options.Applicative (metavar)



newtype GetTestSpectrum = GetTestSpectrum Bool
  deriving (Eq, Ord, Typeable)

instance IsOption GetTestSpectrum where
    defaultValue = GetTestSpectrum False
    parseValue = fmap GetTestSpectrum . safeReadBool
    optionName = return "get-spectrum"
    optionHelp = return "Runs the test and produces a spectrum"
    optionCLParser = flagCLParser Nothing (GetTestSpectrum True)

newtype HpcDir = HpcDir String
  deriving (Eq, Ord, Typeable)

instance IsOption HpcDir where
    defaultValue = HpcDir ".hpc"
    parseValue = Just . HpcDir
    optionName = return "hpc-dir"
    optionHelp = return "Directory where HPC mix files are located"
    optionCLParser = mkOptionCLParser (metavar "HPCDIRECTORY")

data SpectrumOut = PrintSpectrum 
                 | SaveSpectrum { spectrum_loc :: String}

instance IsOption SpectrumOut where
    defaultValue = PrintSpectrum
    parseValue = Just . SaveSpectrum 
    optionName = return "spectrum-out"
    optionHelp = return "Spectrum output file"
    optionCLParser = mkOptionCLParser (metavar "CSVOUT")

data SpectrumResult = SpecRes {
          tix_module :: [TixModule],
          test_result :: Bool,
          test_name :: String
        }



specResRow :: SpectrumResult -> (String, Bool, [(String, [Integer])])
specResRow (SpecRes{..})=
  (test_name, test_result, map simpleRep tix_module)
  where simpleRep :: TixModule -> (String, [Integer])
        simpleRep tm = (tixModuleName tm, tixModuleTixs tm)


third :: (a, b, c) -> c
third (_,_,c) = c

testSpectrum :: Ingredient
testSpectrum = TestManager [Option (Proxy :: Proxy GetTestSpectrum),
                            Option (Proxy :: Proxy Timeout),
                            Option (Proxy :: Proxy HpcDir),
                            Option (Proxy :: Proxy SpectrumOut)] $
  \opts tree ->
    case lookupOption opts of
      GetTestSpectrum False -> Nothing
      GetTestSpectrum True -> Just $ do
         let ts = unfoldTastyTests tree
             timeout :: Timeout
             timeout = lookupOption opts
             hpc_dir = case lookupOption opts of
                            HpcDir str -> str
         spectrums <- forM ts $ \(t_name,test) -> do
            clearTix
            t_res <- checkTastyTree timeout test
            Tix res <- examineTix
            return (SpecRes res t_res t_name)
         let all_mods = Set.unions $ map ((Set.fromList . map fst . third) . specResRow) spectrums
         mixes <- Map.fromList <$>
                      mapM (\m ->(m,) . (\(Mix fp _ _ _ mes) -> (fp,map fst mes))
                          <$> readMix [hpc_dir] (Left m)) (Set.toList all_mods)
         let toCanonicalExpr fp hp = fp ++ ':' : show hp
             all_exprs = Set.fromList $ concatMap f $ Map.elems mixes
               where f (fp, hpcs) = map (toCanonicalExpr fp) hpcs
             toFullRow sp = (tn, tr, Map.fromList $ concatMap rowToFull tre)
               where (tn, tr, tre) = specResRow sp
                     rowToFull (mod, tix) = zip (map (toCanonicalExpr fp) hpcs)
                                             $ filter (/= 0) tix
                        where (fp, hpcs) = mixes Map.! mod
             toRes sp = (t_name, t_res, map fwd $ Set.elems all_exprs )
              where (t_name, t_res, spec) = toFullRow sp
                    fwd k = Map.findWithDefault 0 k spec

{-
                    test status (t_res). A string? Leave it bool for now.
                       ↓
         test_name, test_result  , e1 , e2 <- mix_file location
 test_name <-   t1,   y (1/1)    , 1  , 27 <- number of evals
                t2,   y          , 0  , 17
                t3,   n (0/1)    , 17 , 5
                t4,   n          , 5  , 0
                q1,   n (4/5)    , 7  , 5
                        ↑
                      from amount of tests, if < 1 then fail

think about data format..?

-}
         let header = "test_name, test_result, " ++ intercalate ", " (map show $ Set.elems all_exprs)
         let printFunc (s,b,e) =
                show s ++ ", " ++ show b  ++ ", " ++ intercalate ", " (map show e)
             csv = header:map (printFunc . toRes) spectrums

         case lookupOption opts of
            PrintSpectrum -> do
                void $ mapM putStrLn csv
            SaveSpectrum fp -> 
                writeFile fp $ intercalate "\n" csv ++ "\n"

         return $ all test_result spectrums



unfoldTastyTests :: TestTree -> [(String, TestTree)]
unfoldTastyTests = TR.foldTestTree (TR.trivialFold {TR.foldSingle = fs'}) mempty
  where
    fs' opts name test = [(name, TR.PlusTestOptions (opts <>) $ TR.SingleTest name test)]


checkTastyTree :: Timeout -> TestTree -> IO Bool
checkTastyTree timeout test =
  case tryIngredients [TestReporter [] (\_ _ -> Just reportFun)] mempty with_timeout of
    Just act -> act
    _ -> return False
  where
    with_timeout = localOption timeout test
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
