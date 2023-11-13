{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Tasty.Ingredients.Spectrum.ParseCSV where

import Test.Tasty.Ingredients.Spectrum.Types

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (mapMaybe)
import Data.List (transpose, group, sort)

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM
import Data.IntMap (IntMap)

import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AT

                            
attoParse :: Parser ([(String, (Int,Int,Int,Int))], -- Locs
                     [((String,String),
                       Bool, [Integer])]) 
attoParse = do locs <- parseHeader
               endOfLine
               entries <- sepBy1 (parseEntry (length locs)) endOfLine
               -- endOfInput
               return (locs, entries)
  where parseHeader = string "test_name,test_type,test_result,"
                      >> sepBy1 parseLoc (char ',')
          where parseLoc :: Parser (String ,(Int,Int,Int,Int))
                parseLoc = do char '"'
                              fn <- manyTill anyChar (char ':')
                              l1 <- manyTill anyChar (char ':')
                              c1 <- manyTill anyChar (char '-')
                              l2 <- manyTill anyChar (char ':')
                              c2 <- manyTill anyChar (char '"')
                              return (fn, (read @Int l1, read @Int c1,
                                           read @Int l2, read @Int c2))
        parseEntry :: Int -> Parser ((String,String), Bool, [Integer])
        parseEntry num = do t_name <- parseString
                            char ','
                            t_type <- parseString
                            char ','
                            t_res <- parseBool 
                            evals <- count num (char ',' >> signed decimal)
                            return ((T.unpack t_name, T.unpack t_type), t_res, evals)
        
        parseBool :: Parser Bool
        parseBool = choice [string "True" >> return True,
                            string "False" >> return False]
        parseString :: Parser T.Text
        -- We just take until we get an unescaped '"'
        parseString = do char '"'
                         (t, _) <- match $ 
                                    manyTill anyChar (satisfy (/= '\\') >> char '"')
                         case T.unsnoc t of -- O(1)
                            Just (t', '"') -> return t'
                            _ -> fail "Unmatched string!"
                                     

parseCSV :: FilePath -> IO ([((String,String), Bool)], IM.IntMap String, [Label])
parseCSV target_file = do
          f <- TIO.readFile target_file
          let p_res = parseOnly attoParse f
          (locs, parsed) <- case p_res of
                             Left err -> error ("Got error while parsing :" <>  err)
                             Right r -> return r
          let groups = map head $ group $ sort $ map (\(s,_) -> s) $ locs

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


