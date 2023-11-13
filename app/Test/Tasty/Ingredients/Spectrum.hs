{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
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

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Control.Monad
import Data.List (intercalate)
import Options.Applicative (metavar)

import Data.IORef



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

newtype SparseSpectrum = SparseSpectrum Bool
  deriving (Eq, Ord, Typeable)

instance IsOption SparseSpectrum where
    defaultValue = SparseSpectrum True
    parseValue = fmap (SparseSpectrum . not) . safeReadBool
    optionName = return "non-sparse-spectrum"
    optionHelp = return $  "Create a *non-sparse spectrum*, keeping all "++
                           "expressions, even those that are never evaluated "++
                           "in any test."
    optionCLParser = flagCLParser Nothing (SparseSpectrum False)


-- | Primary function of the module - enables the import at `defaultMainWithIngredients` for other tasty test projects.
-- See the [Tasty Repository](https://github.com/UnkindPartition/tasty) for more information on ingredients.
-- Important: running this spectrum will re-run all tests, which might take a time. Also, the resulting `.tix` file will not be representive of your whole test-suite. 
-- If you need coverage information, you need to retrieve it from a different run. 
testSpectrum :: Ingredient
testSpectrum = TestManager [Option (Proxy :: Proxy GetTestSpectrum),
                            Option (Proxy :: Proxy Timeout),
                            Option (Proxy :: Proxy HpcDir),
                            Option (Proxy :: Proxy SpectrumOut),
                            Option (Proxy :: Proxy SparseSpectrum)] $
  \opts tree ->
    case lookupOption opts of
      GetTestSpectrum False -> Nothing
      GetTestSpectrum True -> Just $ do
         -- Step 0: Setup Tests, Arguments, Variables
         let tests = unfoldTastyTests tree
             timeout :: Timeout
             timeout = lookupOption opts
             sparseSpectrum = case lookupOption opts of
                                SparseSpectrum s -> s
             hpc_dir = case lookupOption opts of
                            HpcDir str -> str
        -- Step 1: Delete Tix, Run every test in isolation, Retrieve resulting Tix per Test 
         spectrums <- forM tests $ \(t_name,test) -> do
            clearTix
            t_res <- checkTastyTree timeout test
            let t_type = getTestType test
            Tix res <- examineTix
            -- The results are usually quite sparse, so we use an IntMap here,
            -- and only keep track of the non-zero values. We then re-infer the
            -- zeroes when we generate the output.
            let simpleRep :: TixModule -> (String, IntMap Integer)
                simpleRep tm = (tixModuleName tm, im)
                    where im = IM.fromAscList $ 
                               filter ((/= 0) . snd) $
                               zip [0.. ] $
                               tixModuleTixs tm
                -- The bang here is very important, ensuring we evaluate the new_map here. 
                -- Otherwise we quickly run out of memory on big projects & test-suites.
                !new_map = Map.fromList $
                           filter (not . IM.null . snd) $ map simpleRep res
            return (t_name, t_type, t_res, new_map)
        -- Step 1.1: Reduce the tix to only the touched ones.
         -- We only care about locations that have been touched at any point,
         -- unless we're doing a non-sparse spectrum.
         let touched = Map.unionsWith IS.union $
                       map (\(_,_,_,m) -> Map.map IM.keysSet m) $
                       spectrums
         -- Step 2: Load all mix files from the specified HPC directory
         mixes <- fmap Map.fromList <$>
                    traverse (\(m, touched_inds) ->
                      (m,) . (\(Mix fp _ _ _ mes) ->
                        (fp, let hpcs = map fst mes
                             in if sparseSpectrum
                                then map snd $
                                    filter (flip IS.member touched_inds . fst) $
                                    zip [0..] hpcs
                                else hpcs))
                       <$> readMix [hpc_dir] (Left m)) $ Map.assocs touched
         -- Step 2.1: Resolve the Mix indizes to "speaking names" of file+row+column 
         let toCanonicalExpr file hpc_pos = file ++ ':' : show hpc_pos
         -- Step 3: Merge all results into a string in .csv style
             all_exprs = concatMap to_strings $ Map.elems mixes
               where to_strings (filename, hpcs) =
                        map (toCanonicalExpr filename) hpcs

             toRes (t_name, t_type, t_res, tix_maps) = 
                (t_name, t_type, t_res, concatMap eRes $ Map.assocs mixes)
              where -- eRes :: (String, (String, [HpcPos])) -> [Integer]
                    eRes (mod, (_,hpcs)) = 
                        case tix_maps Map.!? mod of
                            Just im ->
                                map (flip (IM.findWithDefault 0) im) $
                                if sparseSpectrum
                                then let t_inds = touched Map.! mod
                                     in IS.toList $ t_inds
                                else [0.. (length hpcs)]
                            Nothing -> replicate (length hpcs) 0

             header = "test_name,test_result," ++
                       intercalate "," (map show all_exprs)
             printFunc (s,tt,b,e) =
                show s ++ "," ++
                tt ++ "," ++
                show b ++ "," ++
                intercalate "," (map show e)
             csv = map (printFunc . toRes) spectrums
         -- Step 3.1: Print to Console or File, depending on args. 
         case lookupOption opts of
            PrintSpectrum -> do
                putStrLn header
                mapM_ putStrLn csv
            SaveSpectrum fp -> do
                writeFile fp (header ++ "\n")
                mapM_ (appendFile fp . (++ "\n")) csv

         return $ all (\(_,_,r,_) -> r) spectrums

-- | Unfolds a test-collection into single tests. 
-- The resulting tests are still TestTrees, due to tasty logic and keeping things runnable. 
-- TODO: Keeps some more names around
unfoldTastyTests :: 
  TestTree                 -- ^ A collection of Tasty Tests
  -> [(String, TestTree)]  -- ^ A list of (TestName,Test) with single tests without sub-elements.
unfoldTastyTests = TR.foldTestTree (TR.trivialFold {TR.foldSingle = fs,
                                                    TR.foldGroup = fg}) mempty
  where fs opts name test = [(name, TR.PlusTestOptions (opts <>) $
                                     TR.SingleTest name test)]
        fg gopts gname = map (\(n,t) -> (gname<>"/"<>n, TR.PlusTestOptions (gopts <>) $ t))


-- | This function runs a single test - but a single test and a test-collection share the same type in tasty. 
-- The result is the test-status. True on pass, false on fail or error.
-- The "tix" are retrieved separately after this function was executed.  
checkTastyTree :: 
  Timeout       -- ^ Timeout how long the test will be run atmost - if timeout is reached the test is errore`d.
  -> TestTree   -- ^ Executable Tasty Test Tree - meant to be a single test!
  -> IO Bool    -- ^ Test Result. True on pass, False on fail,timeout or error
checkTastyTree timeout test =
  case tryIngredients [TestReporter []
                       (\_ _ -> Just reportFun)] mempty with_timeout of
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

getTestType :: TestTree -> String
getTestType (TR.TestGroup _ _) = "GROUP"
getTestType (TR.SingleTest name t) = show (typeOf t) 
getTestType (TR.PlusTestOptions _ t) = getTestType t
getTestType _ = "UKNOWN/OTHER" 
