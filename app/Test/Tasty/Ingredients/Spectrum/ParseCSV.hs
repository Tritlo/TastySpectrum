{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternGuards #-}
module Test.Tasty.Ingredients.Spectrum.ParseCSV where

import Test.Tasty.Ingredients.Spectrum.Types

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (mapMaybe)
import Data.List (transpose, group, sort)

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM


parseCSV :: FilePath -> IO ([(String, Bool)], IM.IntMap String, [Label])
parseCSV target_file = do
          f <- TIO.readFile target_file
          let (h:rs) = T.splitOn (T.pack "\n") f
              (_:_:locs) = map ((\(fn,l) -> ( T.unpack $ T.drop 1 fn,
                                             fromHpcPos $ read @HpcPos
                                                        $ T.unpack
                                                        $ T.dropEnd 1
                                                        $ T.drop 1 l))
                                            . T.breakOn (T.pack ":") ) $
                                            (T.splitOn $ T.pack ",") h
              -- We need to take care with test-names, as they might have
              -- commas in them.
              parseLine ln | (t_name,rs) <- (T.span (/= '"') . T.drop 1) ln,
                             (_:t_res:evals) <- (T.splitOn $ T.pack ",") rs,
                             n <- T.unpack t_name,
                             b <- read @Bool $ T.unpack t_res,
                             e <- map (read @Integer . T.unpack) evals
                            = Just (n,b,e)
                           | T.null ln = Nothing
                           | otherwise = error ("could not parse " <> T.unpack ln)

              parsed :: [(String, Bool, [Integer])]
              parsed = mapMaybe parseLine rs
                
              groups = map head $ group $ sort $ map (\(s,_) -> s) $ locs

              findGroup = Map.fromAscList $ zip groups [0..]

              loc_groups = IM.fromAscList $ zip [0..] groups

              test_results = map (\(n,r,_) -> (n,r)) parsed
              eval_results = transpose $ map (\(_,r,e) ->
                                                if r
                                                then e
                                                else map negate e) parsed

              keepNonZero :: [Integer] -> [(Int,Integer)]
              keepNonZero = filter ((/=0) . snd) . zip [0..]
              labeled = filter (\(Label _ _ _ v) -> not $ null v) $
                          zipWith3 (\(s,l) i es -> Label (findGroup Map.! s)
                            l i $ keepNonZero es) locs [0..] eval_results

          return (test_results, loc_groups, labeled)


