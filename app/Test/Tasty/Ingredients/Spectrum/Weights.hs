{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}

module Test.Tasty.Ingredients.Spectrum.Weights where

import Control.Monad (replicateM)
import Data.List (foldl', sortOn)
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Debug.Trace
import GHC.IOArray
import qualified Numeric.AD.Mode.Reverse as AD
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC
import Trace.Hpc.Util

data ModuleResult = MR FilePath [(HpcPos, [Double])]
    deriving (Show)

weightsParse ::
    FilePath ->
    IO
        ( [Text] -- Rules
        , [ModuleResult]
        )
weightsParse fp = do
    (_ : file) <- T.lines <$> T.readFile fp
    let (rule_txts, (_ : _ : rest)) = span (not . T.null) file
        rules = map (T.drop 2) rule_txts
        num_rules = length rules
        modResChunking [] = []
        modResChunking (a : b : r) = (a, b) : (modResChunking r)
        mod_res_chunks = modResChunking rest
        parseChunk (fp_text, ws_text) = MR (T.unpack fp) $ parseWs ws
          where
            fp = T.drop 2 $ T.dropEnd 1 fp_text
            ws = T.drop 4 $ ws_text
        parseWs ws_t = parseWs' ws
          where
            ws = T.drop 1 $ T.dropEnd 1 $ ws_t
            parseWs' ws
                | (w, r) <- T.span (/= ')') ws
                , r' <- T.drop 2 $ r
                , w' <- parseW (T.drop 1 w) =
                    if T.null r'
                        then [w']
                        else w' : (parseWs' r')
            parseW w
                | (hp, r) <- T.span (/= ',') w
                , r' <- T.drop 2 $ T.dropEnd 1 r
                , hp' <- T.drop 1 $ T.dropEnd 1 hp
                , rs <- T.splitOn (T.pack ",") r' =
                    ( read $ T.unpack hp'
                    , map (read . T.unpack) rs
                    )
    return (rules, map parseChunk mod_res_chunks)

data Neuron a = N (a, [a])
    deriving (Show, Functor, Foldable, Traversable)

data Layer a = L [Neuron a]
    deriving (Show, Functor, Foldable, Traversable)

data MLP a = MLP [Layer a]
    deriving (Show, Functor, Foldable, Traversable)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = as : (chunksOf n bs)
  where
    (as, bs) = splitAt n xs

shuffle :: MWC.GenIO -> [a] -> IO [a]
shuffle g v = do
    arr <- newIOArray (0, length v - 1) undefined
    mapM_ (uncurry $ writeIOArray arr) $ zip [0 ..] v
    mapM_
        ( \fi -> do
            to_i <- MWC.uniformR (fi, length v - 1) g
            fe <- readIOArray arr fi
            te <- readIOArray arr to_i
            writeIOArray arr to_i fe
            writeIOArray arr fi te
        )
        [0 .. length v - 2]
    mapM (readIOArray arr) [0 .. length v - 1]

-- [2024-01-16] Inspired by Karpathy's micrograd, based on
-- Quick and dirty backpropagation in Haskell by Mazzo
-- (https://mazzo.li/posts/haskell-backprop-short.html)
machineLearn :: LearnParams -> [(String, Bool, [Double])] -> IO [(String, Double)]
machineLearn (LP network chunk_size l_r_0) inps = do
    g <- MWC.createSystemRandom
    mlp <- initMLP g network
    batches <- chunksOf chunk_size <$> (shuffle g samples)
    putStrLn $ "Epochs: " ++ show (length batches)
    let optimized_mlp = optimize mlp batches
    putStrLn ("Final loss:" ++ show (loss optimized_mlp samples))
    putStrLn ("Final accuracy:" ++ show (accuracy optimized_mlp samples))
    return $ map (\(s, _, w) -> (s, callMLP optimized_mlp w)) inps
  where
    all_ws = L.transpose $ map (\(_, _, w) -> w) inps
    min_w = map minimum all_ws
    max_w = map maximum all_ws
    range_w = zipWith (-) max_w min_w
    normalize = flip (zipWith (/)) range_w

    samples :: [([Double], Double)]
    samples =
        map
            ( \(s, b, w) ->
                if b
                    then (normalize w, 1)
                    else (normalize w, -1)
            )
            inps
    callNeuron ::
        (Num a) => -- we can't specialize to double,
        -- because this is later used with AD (Reverse)
        [a] ->
        (a -> a) ->
        Neuron a ->
        a
    callNeuron xs activate (N (bias, weights)) =
        activate (bias + (sum $ zipWith (*) weights xs))
    -- type Layer :: [Neuron] = [(Double,[Double])]
    callLayer :: (Num a) => [a] -> (a -> a) -> Layer a -> [a]
    callLayer inputs activation (L neurons) =
        map (callNeuron inputs activation) neurons
    reLU :: (Num a, Ord a) => a -> a
    reLU x = if x > 0 then x else 0

    callMLP :: (Num a, Ord a) => MLP a -> [a] -> a
    callMLP (MLP layers) inputs =
        head $
            callLayer
                (foldl' (\xs -> callLayer xs reLU) inputs (init layers))
                id
                (last layers)

    initNeuron :: MWC.GenIO -> Int -> IO (Neuron Double)
    initNeuron g num_inputs =
        (N . (0,)) <$> replicateM num_inputs (MWC.uniformR (-1, 1) g)

    initLayer :: MWC.GenIO -> Int -> Int -> IO (Layer Double)
    initLayer g num_inputs num_outputs =
        L <$> replicateM num_outputs (initNeuron g num_inputs)

    initMLP :: MWC.GenIO -> [Int] -> IO (MLP Double)
    initMLP g input_nums =
        MLP
            <$> traverse
                (uncurry (initLayer g))
                (zip input_nums (tail input_nums ++ [1]))

    -- TODO: this is just from Karpathy's moon example.
    -- Probably something better is there.
    -- Is this loss?
    loss :: (Fractional a, Ord a) => MLP a -> [([a], a)] -> a
    loss mlp batch = data_loss + reg_loss
      where
        mlp_outs = map (callMLP mlp . fst) batch
        losses =
            zipWith
                ( \(_, label) mlp_out ->
                    reLU (1 + (-label) * mlp_out)
                )
                batch
                mlp_outs
        data_loss = sum losses / fromIntegral (length losses)
        -- L2 regularization
        alpha = 1e-4
        reg_loss = alpha * sum (fmap (\p -> p * p) mlp)

    accuracy :: (Num a, Ord a) => MLP a -> [([a], a)] -> Double
    accuracy mlp samples =
        (fromIntegral $ length $ correct)
            / (fromIntegral $ length samples)
      where
        correct = filter (\(s, l) -> signum (callMLP mlp s) == signum l) samples

    optimizeStep ::
        MLP Double ->
        [([Double], Double)] -> -- samples
        Double ->
        MLP Double
    optimizeStep mlp batch learning_rate =
        AD.gradWith
            (\x dx -> x - learning_rate * dx)
            ( \ad_mlp ->
                loss
                    ad_mlp
                    (map (\(is, o) -> (map AD.auto is, AD.auto o)) batch)
            )
            mlp
    optimize :: MLP Double -> [[([Double], Double)]] -> MLP Double
    optimize mlp0 batches =
        foldl'
            ( \mlp (epoch, batch) ->
                ( if epoch `mod` 100 == 0
                    then
                        ( traceShow
                            ( "Epoch, Loss, Accuracy:"
                            , epoch
                            , loss mlp samples
                            , accuracy mlp samples
                            )
                        )
                    else id
                )
                    $ let l_r = (1.0 - l_r_0 * fromIntegral epoch / fromIntegral ne)
                       in optimizeStep mlp batch l_r
            )
            mlp0
            (zip [0 ..] batches)
      where
        ne = length batches

data LearnParams = LP
    { network :: [Int]
    , chunk_size :: Int
    , l_r_0 :: Double
    }
    deriving (Show, Read)

data Parameters = Params
    { bug_locs :: [(FilePath, [(Int, Int)])]
    , machine_parameters :: LearnParams
    }
    deriving (Show, Read)

-- [2024-01-17] Invoked with

-- $ cabal run tasty-sbfl -- p1-results weights p1-params

runWeights :: FilePath -> FilePath -> IO ()
runWeights parameters fp =
    do
        (rules, ws) <- weightsParse fp
        -- [2024-01-17] We read the parameters from a file.
        -- To generate params, load the repl, create a Parameters object
        -- and write it to a file, e.g.
        -- > writeFile "p1-params" $ show $
        --  Params { bug_locs = [("src/Text/Pandoc/Writers/Docbook.hs",[(400,500)])],
        --           machine_parameters = LP { network = [11,22,22],
        --                                     chunk_size = 100,
        --                                     l_r_0 = 0.9}}
        Params buggy mp <- read <$> readFile parameters

        let non_infinite =
                map
                    ( \(MR fp w) ->
                        MR fp $
                            filter
                                ( \(_, pw) ->
                                    not $ any isInfinite pw
                                )
                                w
                    )
                    ws
            bug_map :: Map FilePath [HpcPos]
            bug_map = Map.fromList $ map (\(f, ls) -> (f, map top ls)) buggy
              where
                top (ls, le) = toHpcPos (ls, 0, le, maxBound)
            labelData :: ModuleResult -> [(String, Bool, [Double])]
            labelData (MR fp ws) =
                map (\(p, w) -> ((fp ++ ":" ++ show p), lUp fp p, w)) ws
              where
                lUp fp l = case bug_map Map.!? fp of
                    Just locs -> any (insideHpcPos l) $ locs
                    _ -> False
            labeled_data = concatMap labelData non_infinite

        res <- machineLearn mp labeled_data
        mapM_ print $ take 20 $ sortOn (negate . snd) res
