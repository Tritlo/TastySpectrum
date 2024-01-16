{-# LANGUAGE BangPatterns, TupleSections #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Test.Tasty.Ingredients.Spectrum.Weights where 


import Trace.Hpc.Util

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.List (sortOn, foldl')
import qualified Data.Map.Strict as Map
import Data.Map (Map)

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC
import Control.Monad (replicateM)

import qualified Numeric.AD.Mode.Reverse as AD
import Debug.Trace
import GHC.IOArray


data ModuleResult = MR FilePath [(HpcPos, [Double])] 
    deriving (Show)

weightsParse :: FilePath -> IO ([Text], -- Rules
                                [ModuleResult])
weightsParse fp = do
    (_:file) <- T.lines <$> T.readFile fp
    let (rule_txts, (_:_:rest)) = span (not . T.null) file
        rules = map (T.drop 2) rule_txts
        num_rules = length rules
        modResChunking [] = []
        modResChunking (a:b:r) = (a,b):(modResChunking r)
        mod_res_chunks = modResChunking rest
        parseChunk (fp_text,ws_text) = MR (T.unpack fp) $ parseWs ws
         where fp = T.drop 2 $ T.dropEnd 1 fp_text
               ws = T.drop 4 $ ws_text
        parseWs ws_t = parseWs' ws
            where
              ws = T.drop 1 $ T.dropEnd 1 $ ws_t
              parseWs' ws | (w,r) <- T.span (/= ')') ws,
                            r' <- T.drop 2 $ r,
                            w' <- parseW (T.drop 1 w)
                  = if T.null r' then  [w']
                    else w':(parseWs' r')
              parseW w | (hp, r) <- T.span (/= ',') w,
                         r' <- T.drop 2 $ T.dropEnd 1 r,
                         hp' <- T.drop 1 $ T.dropEnd 1 hp,
                         rs <- T.splitOn (T.pack ",") r'
                    = (read $ T.unpack hp',
                       map (read . T.unpack) rs)
    return (rules, map parseChunk mod_res_chunks)


data Neuron a = N (a, [a])
 deriving (Show, Functor, Foldable, Traversable)

data Layer a = L [Neuron a]
 deriving (Show, Functor, Foldable, Traversable)


data MLP a = MLP [Layer a]
 deriving (Show, Functor, Foldable, Traversable)


chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = as:(chunksOf n bs)
  where (as, bs) = splitAt n xs

shuffle :: MWC.GenIO -> [a] -> IO [a]
shuffle g v = do
  arr <- newIOArray (0, length v -1) undefined 
  mapM_ (uncurry $ writeIOArray arr) $ zip [0..] v
  mapM_ (\fi -> do to_i <- MWC.uniformR (fi, length v - 1) g
                   fe <- readIOArray arr fi
                   te <- readIOArray arr to_i
                   writeIOArray arr to_i fe
                   writeIOArray arr fi te) [0 .. length v - 2]
  mapM (readIOArray arr) [0.. length v -1]

-- [2024-01-16] Inspired by Karpathy's micrograd, based on 
-- Quick and dirty backpropagation in Haskell by Mazzo
-- (https://mazzo.li/posts/haskell-backprop-short.html)
machineLearn :: [(String, Bool, [Double])] -> IO [(String, Double)]
machineLearn inps@((_,_,ws):_) = do
    g <- MWC.createSystemRandom
    mlp <- initMLP g [length ws, 16, 16]
    batches <- chunksOf 1000 <$> (shuffle g samples)
    putStrLn $ "Epochs: "  ++ show (length batches)
    let optimized_mlp = optimize mlp $ chunksOf 1000 samples
    return $ map (\(s,_,w) -> (s, callMLP optimized_mlp w)) inps
   where
     samples :: [([Double], Double)]
     samples = map (\(s,b,w) -> if b then (w,1) else (w,-1)) inps
     callNeuron :: Num a -- we can't specialize to double,
                         -- because this is later used with AD (Reverse)
                => [a] -> (a -> a) -> Neuron a -> a
     callNeuron xs activate (N (bias, weights)) =
       activate (bias + (sum $ zipWith (*) weights xs))
     -- type Layer :: [Neuron] = [(Double,[Double])]
     callLayer :: Num a => [a] -> (a -> a) -> Layer a -> [a]
     callLayer inputs activation (L neurons) =
         map (callNeuron inputs activation) neurons
     reLU :: (Num a, Ord a) => a -> a
     reLU x = if x > 0 then x else 0

     callMLP :: (Num a, Ord a) => MLP a -> [a] -> a
     callMLP (MLP layers) inputs =
       head $ callLayer 
              (foldl' (\xs -> callLayer xs reLU) inputs (init layers))
              id
              (last layers)

     initNeuron :: MWC.GenIO -> Int -> IO (Neuron Double)
     initNeuron g num_inputs =
         (N . (0,)) <$> replicateM num_inputs (MWC.uniformR (-1,1) g)

     initLayer :: MWC.GenIO -> Int -> Int -> IO (Layer Double)
     initLayer g num_inputs num_outputs =
         L <$> replicateM num_outputs (initNeuron g num_inputs)

     initMLP :: MWC.GenIO -> [Int] -> IO (MLP Double)
     initMLP g input_nums =
         MLP <$> traverse (uncurry (initLayer g))
                          (zip input_nums (tail input_nums ++ [1]))

     -- TODO: this is just from karpathys moon example.
     -- Probably something better is there.
     -- Is this loss?
     loss :: (Fractional a, Ord a) => MLP a -> [([a],a)] -> a
     loss mlp samples = data_loss + reg_loss
       where mlp_outs = map (callMLP mlp . fst) samples
             losses = zipWith (\(_,label) mlp_out ->
                             reLU (1 + (- label) * mlp_out))
                         samples mlp_outs
             data_loss = sum losses / fromIntegral (length losses)
             -- L2 regularization
             alpha = 1e-4
             reg_loss = alpha * sum (fmap (\p -> p*p) mlp)
                             -- without Functor/Foldable:
                             -- (map (sum .
                             --      map (sum .
                             --      map (\p->p*p) . snd)) mlp)

     optimizeStep :: MLP Double
                  -> [([Double],Double)]  -- samples
                  -> Double
                  -> MLP Double
     optimizeStep mlp batch learning_rate =
         AD.gradWith (\x dx -> x - learning_rate*dx) 
                     (\ad_mlp ->
                         loss 
                           ad_mlp
                           (map (\(is, o) -> (map AD.auto is, AD.auto o)) batch))
                     mlp
     optimize :: MLP Double -> [[([Double],Double)]] -> MLP Double
     optimize mlp0 batches =
         foldl' (\mlp (epoch, batch) ->
                 traceShow ("Epoch, Loss:", epoch, loss mlp batch) $
                 optimizeStep mlp batch (1.0 - 0.9 * (fromIntegral epoch)/(fromIntegral ne)))
               mlp0 
               (zip [0..] batches)
       where ne = length batches
       
            
        
       



runWeights :: FilePath -> IO ()
runWeights fp =
  do (rules, ws) <- weightsParse fp
        
     -- TODO: read this from a file, this is a test for p1
     let buggy :: [(FilePath, [(Int,Int)])]
         -- not there in the file uff
         buggy = [("src/Text/Pandoc/Writers/Docbook.hs", [(400,500)])]
     
                         
     let non_infinite = map (\(MR fp w) -> MR fp $ filter (\(_,pw)->
                                               not $ any isInfinite pw) w) ws
         bug_map :: Map FilePath [HpcPos]
         bug_map = Map.fromList $ map (\(f,ls) -> (f, map top ls)) buggy
            where top (ls,le) = toHpcPos (ls, 0, le, maxBound)
         -- rand_gen = mkStdGen 79934280 -- Chosen by a fair dice-roll.
         -- init_weights = take (length rules) $ randomRs (-1,1)  rand_gen
         -- reduce :: [Double] -> Double
         -- reduce = sum .  zipWith (*) init_weights
         labelData :: ModuleResult -> [(String, Bool, [Double])]
         labelData (MR fp ws) = 
            map (\(p,w) ->  ((fp ++ ":" ++ show p), lUp fp p,  w)) ws
           where lUp fp l = case bug_map Map.!? fp of
                                Just locs -> any (insideHpcPos l) $ locs
                                _ -> False
         labeled_data = concatMap labelData non_infinite

     res <- machineLearn labeled_data
     mapM_ print $ take 20 $ sortOn (negate . snd)  res
     

     




