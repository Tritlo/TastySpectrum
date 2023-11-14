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
    let (!label_map, _, !parents_and_children) = genParentsAndChildren labels
        !env = Env {test_results= IM.fromAscList $ zip [0..] test_results,
                    parents_and_children=parents_and_children,
                    label_map=label_map}
        rules = [rTFail, rTPass,
                rTFailFreq, rTPassFreq,
                rTFailUniqueBranch,
                rTFailFreqDiffParent] 

    mapM print $ map (\l -> map (\r -> r env l) rules) labels 

    error "Rules run!"


data Environment = Env {
                     test_results :: !(IntMap ((String,String),Bool, IntSet)),
                     parents_and_children :: !(IntMap ([Int], IntSet)),
                     label_map :: !(IntMap Label)
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

rTFailFreq :: Rule
rTFailFreq (Env{..}) (Label{..}) = 0.0
    
rTPassFreq :: Rule
rTPassFreq (Env{..}) (Label{..}) = 0.0

rTFailUniqueBranch :: Rule
rTFailUniqueBranch (Env{..}) (Label{..}) = 0.0

rTFailFreqDiffParent :: Rule
rTFailFreqDiffParent (Env{..}) (Label{..}) = 0.0

