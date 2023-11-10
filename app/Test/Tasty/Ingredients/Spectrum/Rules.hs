{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Test.Tasty.Ingredients.Spectrum.Rules where


import Test.Tasty.Ingredients.Spectrum.Types
import Test.Tasty.Ingredients.Spectrum.GenForest

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import Data.Map (Map)

import qualified Data.IntMap.Strict as IM
import Data.IntMap (IntMap)

import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import qualified Data.List as L

import Data.Tree (drawForest)
import Data.Tree
import Data.Maybe (isJust)

runRules :: TestResults -> IO ()
runRules tr@(!test_results, !loc_groups, !labels) = do
    let !imap = IM.fromAscList $ map (\l@Label{loc_index=li} -> (li, l)) labels
        parents (Label{loc_pos=p,loc_index=i}) =
                IS.fromAscList $ map loc_index $
                        filter (\l@Label{loc_pos=lp, loc_index= li} ->
                                        insideHpcPos hp (toHpcPos lp) && li /= i) labels
            where hp = toHpcPos p
        children (Label{loc_pos=p,loc_index=i}) =
                IS.fromAscList $ map loc_index $
                        filter (\Label{loc_pos=lp, loc_index= li} ->
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

        toTree i = Node (prettyLabel loc_groups $ (imap IM.! i)) $ map toTree $
                       IS.toList $ snd (parent_and_direct_children  IM.! i)
         
        
                  
    putStrLn $ drawForest $ map (toTree . fst) roots


-- TODO: we should allow rules to change the environment.
data Environment = Env {
                     test_results :: [(String, Bool)],
                     parent :: IntMap Int,
                     children :: IntMap IntSet 

                   }

type Rule = Environment -> Label -> Double


rTFail :: Rule
rTFail _ Label{..} = 
  fromInteger $ x * (toInteger $ length (filter (< 0) $ IM.elems loc_evals))
    where x = 3

rTPass :: Rule
rTPass _ Label{..} = 
  fromInteger $ (-y) * ( toInteger $ length (filter (>0) $ IM.elems loc_evals))
   where y = 5

