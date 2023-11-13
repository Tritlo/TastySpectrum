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
import Data.IntMap (IntMap)

import Text.ParserCombinators.ReadP


parseLine :: Int -> T.Text -> Maybe ((String,String), Bool, [Integer])
parseLine num ln | T.null ln = Nothing
                 | ((r,rs):rest) <- readP_to_S lineParser (T.unpack ln) =
                    case (rs, rest) of
                        ("",[]) -> Just r
                        _ -> error ("got " <> show (r,rs, rest)
                                     <> " when parsing " <> T.unpack ln)
  where lineParser :: ReadP ((String,String), Bool, [Integer])
        lineParser = do t_name <- readS_to_P (reads @String)
                        char ','
                        t_type <- readS_to_P (reads @String)
                        char ','
                        t_res <- readS_to_P (reads @Bool)
                        -- We know exactly how many integers we are going to parse
                        evals <- count num (char ',' >> (readS_to_P $ reads @Integer))
                        eof
                        return ((t_name, t_type), t_res, evals)


parseHeader :: T.Text -> [(String, (Int,Int,Int,Int))]
parseHeader ln = case (readP_to_S headerParser $ T.unpack ln) of
                   [(r,"")] -> r
                   _ -> error $ "could not parse header " <> (T.unpack ln)
 where headerParser :: ReadP [(String, (Int,Int,Int,Int))]
       headerParser = do string "test_name"
                         char ','
                         string "test_type"
                         char ','
                         string "test_result"
                         char ','
                         loc_strs <- sepBy1 (readS_to_P (reads @String)) (char ',')
                         eof
                         return $ map parseLoc loc_strs
        where parseLoc :: String -> (String, (Int,Int,Int,Int))
              parseLoc inp = (s, fromHpcPos $ read r)
                where (s, _:r) = span (/= ':') inp
                            
                         
                        

parseCSV :: FilePath -> IO ([((String,String), Bool)], IM.IntMap String, [Label])
parseCSV target_file = do
          f <- TIO.readFile target_file
          let (h:rs) = T.splitOn (T.pack "\n") f
              locs = parseHeader h

              parsed :: [((String,String), Bool, [Integer])]
              parsed = mapMaybe (parseLine (length locs)) rs
                
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


