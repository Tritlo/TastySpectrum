{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Tasty.Ingredients.Spectrum.ParseCSV where

import Test.Tasty.Ingredients.Spectrum.Types
import Test.Tasty.Ingredients.Spectrum.Rules (allRuleNames)

import Data.List (group, groupBy, intercalate, sort, sortBy, transpose)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map

import Data.Function (on)

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Semigroup ((<>))

import Control.DeepSeq (deepseq)
import Debug.Trace (traceShowId)
import Control.Monad (forM_)

parseHeader :: T.Text -> [(String, (Int, Int, Int, Int))]
parseHeader !h = case T.stripPrefix "test_name,test_type,test_result," h of
    Just r -> map parseLoc $ T.splitOn "," r
    _ -> error "Invalid header"
  where
    parseLoc :: T.Text -> (String, (Int, Int, Int, Int))
    parseLoc t = case p of
        Just (!fn, t@(!a, !b, !c, !d)) -> (T.unpack fn, t)
        _ -> error $ "invalid loc " <> (T.unpack t)
      where
        p :: Maybe (T.Text, (Int, Int, Int, Int))
        p = do
            ('"', !r) <- T.uncons t
            (!fn, !r) <- return (T.break (== ':') r)
            (':', !r) <- T.uncons r
            (!l1, !r) <- return (T.break (== ':') r)
            (':', !r) <- T.uncons r
            (!c1, !r) <- return (T.break (== '-') r)
            ('-', !r) <- T.uncons r
            (!l2, !r) <- return (T.break (== ':') r)
            (':', !r) <- T.uncons r
            (!c2, '"') <- T.unsnoc r
            return
                ( fn
                ,
                    ( read @Int $ T.unpack l1
                    , read @Int $ T.unpack c1
                    , read @Int $ T.unpack l2
                    , read @Int $ T.unpack c2
                    )
                )

parseEntry :: T.Text -> ((String, String), Bool, [Integer])
parseEntry !t = case p of
    Just ((!tn, !tt), !tr, !evs) -> ((T.unpack tn, T.unpack tt), tr, evs)
    _ -> error $ "invalid entry " <> T.unpack t
  where
    p = do
        (!t_name, !r) <- parseString t
        (',', !r) <- T.uncons r
        (!t_type, !r) <- parseString r
        (',', !r) <- T.uncons r
        (!t_res, !r) <- parseBool r
        (',', !r) <- T.uncons r
        let nums = map (read @Integer . T.unpack) $ T.splitOn "," r
        return ((t_name, t_type), t_res, nums)

    parseBool :: T.Text -> Maybe (Bool, T.Text)
    parseBool t
        | T.isPrefixOf "True" t = (True,) <$> T.stripPrefix "True" t
        | T.isPrefixOf "False" t = (False,) <$> T.stripPrefix "False" t
        | otherwise = error ("invalid bool " <> T.unpack t)
    -- Parses a top level string. Makes sure to not mess up on nested
    -- strings.
    parseString :: T.Text -> Maybe (T.Text, T.Text)
    parseString s = do
        ('"', r) <- T.uncons s
        (str, res) <- go r
        return (str, T.drop 1 res)
      where
        go r =
            let (pf, rs) = T.break (== '"') r
             in if (T.take 1 rs) /= "\""
                    then error $ "unmatched string " <> (T.unpack s)
                    else case T.unsnoc pf of
                        Just (pfc, c) | c == '\\' ->
                            do
                                (r2, res2) <- go $ T.drop 1 rs
                                return (pf <> "\"" <> r2, res2)
                        _ -> return (pf, rs)

-- Print a spectrum back
printSpectrum :: Spectrum -> IO ()
printSpectrum (tests, fns, mods) =
    do
        TIO.putStr "test_name,test_type,test_result,"
        TIO.putStrLn header_info
        TIO.putStrLn ("-,-,-," <> type_info)
        mapM_ (uncurry printTest) $ zip [0 ..] tests
  where
    header_info :: T.Text
    header_info = T.intercalate (T.pack ",") $ map (\(i, rs) -> (tol i) $ map fst rs) lines

    type_info = T.intercalate (T.pack ",") $ map (uncurry tot) tys
    tol :: Int -> [(Int, Int, Int, Int)] -> T.Text
    tol i lns =
        T.intercalate (T.pack ",") $
            map (\t -> T.pack $ show $ fn ++ ":" ++ show (toHpcPos t)) lns
      where
        fn = fns IM.! i
    tot :: Int -> [[String]] -> T.Text
    tot i lns = T.intercalate (T.pack ",") $ map (T.pack . show) lns
    printTest :: Int -> ((String, String), Bool, IntSet) -> IO ()
    printTest t_id ((t_name, t_type), t_res, _) =
        do
            TIO.putStr $ T.pack $ show t_name ++ "," ++ show t_type ++ "," ++ show t_res ++ ","
            TIO.putStrLn $ T.intercalate (T.pack ",") $ map (T.pack . show . abs) t_evals
      where
        t_evals = map (IM.findWithDefault 0 t_id) test_r

    test_r :: [IntMap Integer]
    test_r = map snd $ concatMap snd lines
    lines :: [(Int, [((Int, Int, Int, Int), IntMap Integer)])]
    lines = mapMaybe toLines' $ IM.elems mods
    tys :: [(Int, [[String]])]
    tys = mapMaybe toTys' $ IM.elems mods
    toLines' :: [Label] -> Maybe (Int, [((Int, Int, Int, Int), IntMap Integer)])
    toLines' lbls@(l : _) = Just (loc_group l, fls)
      where
        f l = (loc_pos l, loc_evals l)
        fls :: [((Int, Int, Int, Int), IntMap Integer)]
        fls = map f lbls
    toLines' _ = Nothing
    toTys' :: [Label] -> Maybe (Int, [[String]])
    toTys' lbls@(l : _) = Just (loc_group l, map loc_info lbls)
    toTys' _ = Nothing

-- Compresses a spectrum to the line level
mergeLines :: Spectrum -> IO ()
mergeLines (tests, fns, mods) =
    do
        TIO.putStr "test_name,test_type,test_result,"
        TIO.putStrLn header_info
        mapM_ (uncurry printTest) $ zip [0 ..] tests
  where
    header_info :: T.Text
    header_info = T.intercalate (T.pack ",") $ map (\(i, rs) -> (tol i) $ map fst rs) merged_lines
    tol :: Int -> [(Int, Int)] -> T.Text
    tol i lns =
        T.intercalate (T.pack ",") $
            map (\(f, t) -> T.pack $ show $ fn ++ ":" ++ (show f) ++ ":0-" ++ (show t) ++ ":0") lns
      where
        fn = fns IM.! i
    printTest :: Int -> ((String, String), Bool, IntSet) -> IO ()
    printTest t_id ((t_name, t_type), t_res, _) =
        do
            TIO.putStr $ T.pack $ show t_name ++ "," ++ show t_type ++ "," ++ show t_res ++ ","
            TIO.putStrLn $ T.intercalate (T.pack ",") $ map (T.pack . show) t_evals
      where
        t_evals = map (IM.findWithDefault 0 t_id) test_r

    test_r :: [IntMap Integer]
    test_r = map snd $ concatMap snd merged_lines
    merged_lines :: [(Int, [((Int, Int), IntMap Integer)])]
    merged_lines = map mergeLines' $ IM.elems mods
    mergeLines' :: [Label] -> (Int, [((Int, Int), IntMap Integer)])
    mergeLines' lbls@(l : _) = (loc_group l, fls)
      where
        f l =
            let (ls, _, le, _) = loc_pos l
                li = loc_index l
             in (l, (ls, le))
        fls :: [((Int, Int), IntMap Integer)]
        fls =
            map
                ( \gs@(g : _) ->
                    ( snd g
                    , ( (IM.unionsWith (+) . map (IM.map abs . loc_evals))
                            . map fst
                      )
                        gs
                    )
                )
                $ groupBy ((==) `on` snd)
                $ sortBy (compare `on` snd)
                $ map f lbls

parseTypes :: T.Text -> [[String]]
parseTypes !ts = case T.stripPrefix "-,-,-," ts of
    Just r | T.length r == 0 -> []
    Just r
        | Just ('[', r) <- T.uncons r
        , Just (r, ']') <- T.unsnoc r
        , tys <- T.splitOn (T.pack "],[") r ->
            map parseType tys
    _ -> error "Invalid prefix for location types. Did you run it on a non-typed spectrum?"
  where
    parseType !t | T.length t == 0 = []
    parseType !t
        | Just ('"', t) <- T.uncons t
        , Just (t, '"') <- T.unsnoc t
        , ts <- T.splitOn (T.pack "\",\"") t =
            map (T.unpack . clean) ts
    parseType !t = error ("Invalid type string: " ++ T.unpack t)
    -- Sometimes the types have newlines in them, which we clean away.
    clean = T.replace (T.pack "\\n") (T.pack " ")

parseCSV :: FilePath -> IO Spectrum
parseCSV target_file = do
    (h : ts : rs) <- T.lines <$> TIO.readFile target_file
    let
        -- the labels are already in order per group
        locs_no_types = parseHeader h
        loc_info = parseTypes ts
        parsed_locs = zipWith (\(m, l) !t -> (m, (l, t))) locs_no_types loc_info
        grouped_locs = groupBy ((==) `on` fst) parsed_locs

        loc_groups =
            IM.fromAscList $
                zipWith
                    (\i g -> (i, fst $ head g))
                    [0 ..]
                    grouped_locs

        keepNonZero :: [Integer] -> IntMap Integer
        keepNonZero = IM.fromAscList . filter ((/= 0) . snd) . zip [0 ..]

        eval_results :: [((String, String), Bool, IntMap Integer)]
        eval_results =
            map
                ( ( \(n, r, e) ->
                        ( n
                        , r
                        , let knz = keepNonZero e
                           in if r
                                then knz
                                else negate <$> knz
                        )
                  )
                    . parseEntry
                )
                rs
        test_results = map (\(n, r, es) -> (n, r, IM.keysSet es)) eval_results

        involved loc_i =
            mapMaybe
                ( \(i, (_, r, im)) ->
                    (i,)
                        <$> IM.lookup loc_i im
                )
                $ zip [0 ..] eval_results

    -- [2023-12-31] If we want to see it as it is parsed:
    -- mapM_ (print . (\((s,s2),r,im) -> ((s,s2), r,
    --                                   IM.size im,
    --                                   sum $ IM.elems im))) eval_results

    -- When there are multiple modules, we need to make sure that we
    -- assign a unique index *globally* and not just within the module.
    -- Otherwise the "involved" call will be wrong!
    let grouped_loc_index :: [[(Int, (String, ((Int, Int, Int, Int), [String])))]]
        grouped_loc_index = gli 0 grouped_locs
          where
            gli !i [] = []
            gli !i (xs : ys) = xs' : gli i' ys
              where
                xs' = gli_1 i xs
                i' = i + length xs'
                gli_1 !i (l : ls) = (i, l) : (gli_1 (i + 1) ls)
                gli_1 !i [] = []

        labeled =
            IM.fromList $
                zipWith
                    ( \group_i locs ->
                        (group_i,) $
                            filter (\(Label _ _ _ _ v) -> not $ IM.null v) $
                                map
                                    ( \(loc_i, (s, (l, t))) ->
                                        Label group_i l loc_i t $
                                            IM.fromAscList $
                                                involved loc_i
                                    )
                                    locs
                    )
                    [0 ..]
                    grouped_loc_index
    return (test_results, loc_groups, labeled)

-- Print a spectrum back
printFormulaResults :: IntMap String -> [ModResult] -> IO ()
printFormulaResults loc_groups mrs =
    do
        TIO.putStr "location,"
        TIO.putStrLn header_info
        mapM_ (\MR{..} -> printResult r_loc_group r_result) mrs
  where
    classicFormulas :: [String]
    classicFormulas =
        [ "rJaccard"
        , "rHamming"
        , "rOptimal"
        , "rOptimalP"
        , "rTarantula"
        , "rOchiai"
        , "rDStar2"
        , "rDStar3"
        , "rRogot1"
        ]
    fInds :: Map.Map String Int
    fInds = Map.fromList (zip allRuleNames [0 :: Int ..])
    header_info :: T.Text
    header_info = T.intercalate (T.pack ",") $ fmap T.pack classicFormulas

    tol :: Int -> [(Int, Int, Int, Int)] -> T.Text
    tol i lns =
        T.intercalate (T.pack ",") $
            map (\t -> T.pack $ show $ fn ++ ":" ++ show (toHpcPos t)) lns
      where
        fn = loc_groups IM.! i

    printResult :: Int -> [((Int,(Int, Int, Int, Int), [String]),[Double])] -> IO ()
    printResult r_loc_group r_results =
        forM_ r_results $ \ ((_,loc,_), vals) -> do
            let results = map (\f_nm -> vals !! (fInds Map.! f_nm)) classicFormulas
            TIO.putStr $ T.pack $ show (fn ++ ":" ++ show (toHpcPos loc)) ++ ","
            TIO.putStrLn $ T.intercalate (T.pack ",") $ map (T.pack . show) results
      where
        fn = loc_groups IM.! r_loc_group

