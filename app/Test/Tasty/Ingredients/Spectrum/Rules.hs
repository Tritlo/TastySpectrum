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

    error "Rules run!"


-- TODO: we should allow rules to change the environment.
data Environment = Env {
                     test_results :: ![(String, Bool)],
                     parents_and_children :: !(IntMap ([Int], IntSet))
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

