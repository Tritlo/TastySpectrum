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

import qualified Data.IntMap.Strict as IM
import Data.IntMap (IntMap)

import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import qualified Data.List as L



import Data.Maybe (isJust)

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
genForest :: TestResults -> Forest Label
genForest (_,loc_groups, labels) = map (toTree . fst) roots
  where !imap = IM.fromAscList $ map (\l@Label{loc_index=li} -> (li, l)) labels

        parents (Label{loc_pos=p,loc_index=i,loc_group=g}) =
                IS.fromAscList $ map loc_index $
                        filter (\l@Label{loc_pos=lp, loc_index=li, loc_group=lg} ->
                                        g == lg &&
                                        insideHpcPos hp (toHpcPos lp) && li /= i) labels
            where hp = toHpcPos p
        children (Label{loc_pos=p, loc_group =g, loc_index=i}) =
                IS.fromAscList $ map loc_index $
                        filter (\Label{loc_pos=lp, loc_index=li, loc_group=lg} ->
                                   g == lg &&
                                   insideHpcPos (toHpcPos lp) hp  && li /= i) labels
            where hp = toHpcPos p
        !all_parents_and_children
            = IM.fromAscList $ map (\l@Label{loc_index=li} -> (li, (parents l, children l))) labels 
        direct_parent_and_children li = (direct_parent, direct_children)
            where (ps, cs) = all_parents_and_children IM.! li
                  -- the direct parent is the one whose parents include all
                  -- the parents of this one except itself.
                  !direct_parent = if IS.null ps then Nothing
                    else case (L.sortOn (negate . IS.size . snd) $
                                map (\pi -> (pi, fst $ all_parents_and_children IM.! pi)) $ IS.toList ps) of 
                          ((direct_parent,_):_) -> Just direct_parent
                          _ -> Nothing
                  !direct_children = if IS.null cs then IS.empty
                        else cs IS.\\ (IS.unions $ map (snd .  (all_parents_and_children IM.!)) $ IS.toList cs)
        !parent_and_direct_children = 
            IM.fromAscList $ map (\Label{loc_index=li}
                                -> (li, direct_parent_and_children li)) labels

        roots = filter (\(i,(dp,_)) -> not (isJust dp)) $
                        IM.assocs parent_and_direct_children

        toTree i = Node (imap IM.! i) $ map toTree $
                       IS.toList $ snd (parent_and_direct_children  IM.! i)

