{-# LANGUAGE BangPatterns #-}
module Test.Tasty.Ingredients.Spectrum.GenForest (
        genForest, leafDistances, rootDistances,
        genParentsAndChildren, leafDistanceList

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

import Data.Either (partitionEithers)

import Data.Maybe (isJust, mapMaybe, maybe)


leafDistances :: [Label] -> Map Label Int
leafDistances ls =
    Map.fromList $ zip ls (leafDistanceList ls)

leafDistanceList :: [Label] -> [Int]
leafDistanceList labels =
    map (maybe (error "leafDistanceList: index not found") id .
         flip L.findIndex levels .  IS.member .  loc_index) labels
    where (_, apc, pdc) = genParentsAndChildren labels
          leaves = IM.keysSet $ IM.filter (IS.null . snd) apc
          levels = dists IS.empty leaves
          dists !sf !n =  if IS.null n' then [n] else (n:dists sf' n')
            where n' = (IM.keysSet $
                        IM.filter (not . IS.disjoint  n . snd) pdc) IS.\\ sf'
                  sf' = sf `IS.union` n

                  
rootDistances :: [Label] -> Map Label Int
rootDistances labels =
    Map.fromList $ map (\l@Label{loc_index=li} -> (l, length $ fst $ pdc IM.! li)) labels
    where (_,_,pdc) = genParentsAndChildren labels


genParentsAndChildren :: [Label] -> (IntMap Label, -- Map of loc_index to labels
                                     IntMap (IntSet,IntSet), -- Map of all parents and all children
                                     IntMap ([Int], IntSet)) -- Map of parents in order and direct children
genParentsAndChildren labels = (imap, all_parents_and_children, parent_li_and_children)
  where !imap = IM.fromAscList $ map (\l@Label{loc_index=li} -> (li, l)) labels
        parents_and_children (Label{loc_pos=p,loc_index=i,loc_group=g}) =
            mapMaybe (\Label{loc_pos=lp, loc_index=li, loc_group=lg} ->
                    let hp = toHpcPos p
                        hlp = toHpcPos lp
                    in if g == lg && li /= i
                       then if hp == hlp
                            -- [2023-31-12] Sometimes two locations
                            -- have the exact same location. In this case,
                            -- we assume the *latter* is the child
                            -- and the *former* is the parent.
                            then Just $ if li < i then Left li else Right li
                            else if insideHpcPos hp hlp
                                 then Just (Left li)
                                 else if insideHpcPos hlp hp
                                      then Just (Right li)
                                      else Nothing
                    else Nothing) labels

        !all_parents_and_children
            = IM.fromAscList $
                map (\l@Label{loc_index=li} ->
                    (li,(\(p,c) -> (IS.fromAscList p,
                                    IS.fromAscList c)) $
                                   partitionEithers $
                                   parents_and_children l)) labels

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
        !parent_li_and_children = IM.fromAscList $ 
                    map (\Label{loc_index=li} ->
                            let (dp,dc) = parent_and_direct_children IM.! li
                                dp_li = case dp of
                                         Just pi -> pi:(get_parent_li pi)
                                         Nothing -> []
                            in (li, (dp_li, dc))) $ labels
        get_parent_li i = case parent_and_direct_children IM.! i of
                               (Just p, _) -> p:(get_parent_li p)
                               _ -> []


-- | This function orders a List of Labels (from a spectrum.csv) into multiple trees.
--   This is done by looking if one label contains another (based on source-code spans)
--   and then using this contains relation to build trees around nodes that are not contained by anything (roots).
genForest :: [Label] -> Forest Label
genForest labels = map (toTree . fst) roots
   where (imap, _, pdc) = genParentsAndChildren labels
         roots = filter (\(i,(dp,_)) -> null dp) $ IM.assocs pdc
         toTree i = Node (imap IM.! i) $ map toTree $ IS.toList $ snd (pdc  IM.! i)

