{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns#-}
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

         -- we Just keep a running track of the modules used
         all_mods_ref <- newIORef (Set.empty :: Set String)
         spectrums <- forM ts $ \(t_name,test) -> do
            clearTix
            t_res <- checkTastyTree timeout test
            Tix res <- examineTix
            -- The results are usually quite sparse, so we use an IntMap here,
            -- and only keep track of the non-zero values. We then re-infer the
            -- zeroes when we generate the output.
            let simpleRep :: TixModule -> (String, IntMap Integer)
                simpleRep tm = (tixModuleName tm, im)
                    where tmt = tixModuleTixs tm
                          im =  IM.fromAscList $ filter ((/= 0) . snd) $
                                    zip [0.. ] tmt
                new_map = Map.fromList $ filter (not . IM.null . snd)
                                       $ map simpleRep res
            all_mods_ref `modifyIORef'` (Set.union (Map.keysSet new_map))
            return (t_name, t_res, new_map)

         all_mods <- Set.toList <$> readIORef all_mods_ref

         mixes <- Map.fromList <$>
                  mapM (\m ->(m,) . (\(Mix fp _ _ _ mes) -> (fp,map fst mes))
                       <$> readMix [hpc_dir] (Left m)) all_mods
         let toCanonicalExpr file hpc_pos = file ++ ':' : show hpc_pos
             all_exprs = concatMap to_strings $ Map.elems mixes
               where to_strings (filename, hpcs) =
                        map (toCanonicalExpr filename) hpcs

             toRes (t_name, t_res, tix_maps) = 
                (t_name, t_res, concatMap eRes $ Map.assocs mixes)
              where -- eRes :: (String, (String, [HpcPos])) -> [Integer]
                    eRes (mod, (_,hpcs)) = 
                        case tix_maps Map.!? mod of
                            Just im -> map (flip (IM.findWithDefault 0) im)
                                        [0.. (length hpcs)]
                            Nothing -> replicate (length hpcs) 0

             header = "test_name,test_result," ++
                       intercalate "," (map show all_exprs)
             printFunc (s,b,e) =
                show s ++ "," ++
                show b ++ "," ++
                intercalate "," (map show e)
             csv = map (printFunc . toRes) spectrums


{-               test status (t_res). A string? Leave it bool for now.
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

         case lookupOption opts of
            PrintSpectrum -> do
                putStrLn header
                mapM_ putStrLn csv
            SaveSpectrum fp -> do
                writeFile fp header
                mapM_ (appendFile fp . (++ "\n")) csv

         return $ all (\(_,r,_) -> r) spectrums



unfoldTastyTests :: TestTree -> [(String, TestTree)]
unfoldTastyTests = TR.foldTestTree (TR.trivialFold {TR.foldSingle = fs'}) mempty
  where fs' opts name test = [(name, TR.PlusTestOptions (opts <>) $
                                     TR.SingleTest name test)]


checkTastyTree :: Timeout -> TestTree -> IO Bool
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
