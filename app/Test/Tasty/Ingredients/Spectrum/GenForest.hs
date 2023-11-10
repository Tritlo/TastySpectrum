{-# LANGUAGE BangPatterns #-}
module Test.Tasty.Ingredients.Spectrum.GenForest (
        genForest, leafDistances, rootDistances,
        genNodeSet, containedBy, contains

        ) where


import Test.Tasty.Ingredients.Spectrum.Types
import Test.Tasty.Ingredients.Spectrum.ParseCSV



import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Graph as Graph
import Data.Graph (Tree(..), Forest)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Data.Function (on)

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM

-- Not very efficient?
leafDistances :: [Label] -> Map Label Int
leafDistances labels =
    Map.fromList $ map (\l -> (l, Set.size $ contains nodeSet l)) labels
    where nodeSet = genNodeSet labels

rootDistances :: [Label] -> Map Label Int
rootDistances labels =
    Map.fromList $ map (\l -> (l, Set.size $ containedBy nodeSet l)) labels
    where nodeSet = genNodeSet labels


genNodeSet :: [Label] -> IntMap (Set Label)
genNodeSet all_nodes = IM.fromAscList $
    map (\g@(h:_) -> (loc_group h, Set.fromList g)) $
       L.groupBy ((==) `on` loc_group) $
       L.sortBy (compare `on` loc_group) $ all_nodes

-- How many locations contain this location
containedBy :: IntMap (Set Label) -> Label -> Set Label
containedBy nodeSet n = ns''
    where ns' = Set.delete n (nodeSet IM.! (loc_group n))
          ns'' = Set.filter (insideHpcPos (toHpcPos $ loc_pos n) . toHpcPos . loc_pos) ns'

contains :: IntMap (Set Label) -> Label -> Set Label
contains nodeSet n = ns''
    where ns' = Set.delete n (nodeSet IM.! (loc_group n))
          ns'' = Set.filter (flip insideHpcPos (toHpcPos $ loc_pos n) . toHpcPos . loc_pos) ns'

-- | This function orders a List of Labels (from a spectrum.csv) into multiple trees.
--   This is done by looking if one label contains another (based on source-code spans)
--   and then using this contains relation to build trees around nodes that are not contained by anything (roots).
genForest :: [Label] -> Forest Label
genForest all_nodes = map toTreeF roots_and_children
  where nodeSet :: IntMap (Set Label)
        !nodeSet = genNodeSet all_nodes
        containedBy' = containedBy nodeSet
        contained :: [(Label, Set Label)]
        !contained = map (\n -> (n, containedBy' n))  all_nodes
        roots :: [Label]
        !roots = map fst $ filter (Set.null . snd) contained
        !roots_and_children =
                    map (\r -> (r, map (\(c,s) -> (c, Set.delete r s)) $
                                            filter (Set.member r . snd) contained)) roots
        toTreeF (r, []) = Node r []
        toTreeF (r,!children) = Node r $ map toTreeF r_and_c
            where !rs = map fst $ filter (Set.null . snd) children
                  !r_and_c =
                        map (\r -> (r, map (\(c,s) -> (c, Set.delete r s)) $
                            filter (Set.member r . snd) children)) rs



