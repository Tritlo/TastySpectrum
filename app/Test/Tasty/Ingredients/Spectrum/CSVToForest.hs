{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternGuards #-}
module Test.Tasty.Ingredients.Spectrum.CSVToForest (
        Label(..),
        csvToForest) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (mapMaybe)
import Trace.Hpc.Util (HpcPos, insideHpcPos)

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Graph as Graph
import Data.Graph (Tree(..), Forest)
import Data.List (transpose)
import qualified Data.List as L
import Data.Function (on)
import qualified Data.Map.Strict as Map
import Data.Map (Map)
    

csvToForest :: FilePath -> IO ([(String, Bool)], Forest Label)
csvToForest target_file = do
          f <- TIO.readFile target_file
          let (h:rs) = T.splitOn (T.pack "\n") f
              (_:_:locs) = map ((\(fn,l) -> ( T.unpack $ T.drop 1 fn,
                                             read @HpcPos $ T.unpack $
                                                            T.dropEnd 1 $
                                                            T.drop 1 l))
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
                           | otherwise = Nothing

              parsed :: [(String, Bool, [Integer])]
              parsed = mapMaybe parseLine rs

              test_results = map (\(n,r,_) -> (n,r)) parsed
              eval_results = transpose $ map (\(_,_,e) -> e) parsed
            
              keepNonZero :: [Integer] -> [(Int,Integer)]
              keepNonZero = filter ((/=0) . snd) . zip [0..]
              labeled = zipWith3 (\(s,l) i es -> Label s l i $ keepNonZero es)
                            locs [0..] eval_results
              forest = genForest labeled
          return (test_results, forest)


data Label = Label {loc_name :: String,
                    loc_pos :: HpcPos,
                    loc_index :: Int,
                    loc_evals :: [(Int,Integer)]}

instance Show Label where
    show (Label {..}) = 
        loc_name ++ ":" ++ show loc_pos ++ " " ++ show (loc_evals)

instance Eq Label where
  (==) = (==) `on` loc_index

instance Ord Label where
  compare = compare `on` loc_index


genForest :: [Label] -> Forest Label
genForest all_nodes = map toTreeF roots_and_children
  where nodeSet :: Map String (Set Label)
        nodeSet = Map.fromAscList $
                       map (\g@(h:_) -> (loc_name h, Set.fromList g)) $
                       L.groupBy ((==) `on` loc_name) $
                       L.sortBy (compare `on` loc_name) $  all_nodes
        containedBy n = (n,ns'')
            where ns' = Set.delete n (nodeSet Map.! (loc_name n))
                  ns'' = Set.filter (insideHpcPos (loc_pos n) . loc_pos) ns'
        contained :: [(Label, Set Label)]
        contained = map containedBy all_nodes
        roots :: [Label]
        roots = map fst $ filter (Set.null . snd) contained
        roots_and_children = 
                    map (\r -> (r, map (\(c,s) -> (c, Set.delete r s)) $
                                            filter (Set.member r . snd) contained)) roots

        toTreeF (r, []) = Node r []
        toTreeF (r,children) = Node r $ map toTreeF r_and_c
            where rs = map fst $ filter (Set.null . snd) children
                  r_and_c = 
                        map (\r -> (r, map (\(c,s) -> (c, Set.delete r s)) $
                            filter (Set.member r . snd) children)) rs
                
            

