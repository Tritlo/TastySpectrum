{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Test.Tasty.Ingredients.Spectrum.ParseCSV where

import Test.Tasty.Ingredients.Spectrum.Types

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (mapMaybe)
import Data.List (transpose, group, sort)

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM
import Data.IntMap (IntMap)



parseHeader :: T.Text -> [(String, (Int,Int,Int,Int))]
parseHeader h = case T.stripPrefix ("test_name,test_type,test_result,") h of
                  Just r -> map parseLoc $ T.splitOn "," r
                  _ -> error "Invalid header"
  where parseLoc :: T.Text -> (String, (Int,Int,Int,Int))
        parseLoc t = case p of
              Just (!fn,t@(!a,!b,!c,!d)) -> (T.unpack fn, t)
              _ -> error $ "invalid loc " <> (T.unpack t)
         where p :: Maybe (T.Text, (Int,Int,Int,Int))
               p = do ('"',!r) <- T.uncons t
                      (!fn, !r) <- return (T.break (== ':') r)
                      (':',!r) <- T.uncons r
                      (!l1, !r) <- return (T.break (== ':') r)
                      (':',!r) <- T.uncons r
                      (!c1, !r) <- return (T.break (== '-') r)
                      ('-',!r) <- T.uncons r
                      (!l2, !r) <- return (T.break (== ':') r)
                      (':',!r) <- T.uncons r
                      (!c2,'"') <- T.unsnoc r
                      return (fn, (read @Int $ T.unpack l1,
                                   read @Int $ T.unpack c1,
                                   read @Int $ T.unpack l2,
                                   read @Int $ T.unpack c2))
   
parseEntry :: T.Text -> ((String,String), Bool, [Integer])
parseEntry t = case p of 
                Just ((!tn,!tt),!tr,!evs) -> ((T.unpack tn, T.unpack tt),tr,evs)
                _ -> error $ "invalid entry " <> T.unpack t
  where p = do (!t_name,!r) <- parseString t
               (',', !r) <- T.uncons r
               (!t_type,!r) <- parseString r
               (',', !r) <- T.uncons r
               (!t_res, !r) <- parseBool r
               (',', !r) <- T.uncons r
               nums <- return $ map (read @Integer . T.unpack) $ T.splitOn "," r
               return ((t_name,t_type), t_res, nums)

        parseBool  :: T.Text -> Maybe (Bool, T.Text)
        parseBool t = if T.isPrefixOf "True" t
                      then (True,) <$> T.stripPrefix "True" t
                      else if T.isPrefixOf "False" t
                           then (False,) <$> T.stripPrefix "False" t
                           else error ("invalid bool " <> T.unpack t)
        -- Parses a top level string. Makes sure to not mess up on nested
        -- strings.
        parseString :: T.Text -> Maybe (T.Text, T.Text)
        parseString s = do ('"',r) <- T.uncons s
                           (str,res) <- go r
                           return (str, T.drop 1 res)
             where go r = let (pf, rs) = T.break (== '"') r
                          in if (T.take 1 rs) /= "\""
                             then error $ "unmatched string " <> (T.unpack s)
                             else case T.unsnoc pf of
                                    Just (pfc,c) | c == '\\' ->
                                        do (r2, res2) <- go $ T.drop 1 rs
                                           return (pf <> "\"" <> r2, res2)
                                    _ -> return (pf,rs)
                    

parseCSV :: FilePath -> IO ([((String,String), Bool)], IM.IntMap String, [Label])
parseCSV target_file = do
          (h:rs) <- T.lines <$> TIO.readFile target_file
          let !locs = parseHeader h 
              !parsed = map parseEntry rs
              groups = map head $ group $ sort $ map (\(s,_) -> s) $ locs

              findGroup = Map.fromAscList $ zip groups [0..]

              loc_groups = IM.fromAscList $ zip [0..] groups

              test_results = map (\(n,r,_) -> (n,r)) parsed
              eval_results = transpose $ map (\(_,r,e) ->
                                                if r
                                                then e
                                                else map negate e) parsed

              keepNonZero :: [Integer] -> IntMap Integer
              keepNonZero = IM.fromAscList . filter ((/=0) . snd) . zip [0..]
              labeled = filter (\(Label _ _ _ v) -> not $ IM.null v) $
                          zipWith3 (\(s,l) i es -> Label (findGroup Map.! s)
                            l i $ keepNonZero es) locs [0..] eval_results
          return (test_results, loc_groups, labeled)


