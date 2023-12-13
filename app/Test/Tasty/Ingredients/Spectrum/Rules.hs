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
runRules tr@(test_results, loc_groups, labels) = do
    let isSorted [] = True
        isSorted [_] = True
        isSorted (x:y:xs) = x <= y && isSorted (y:xs)
    -- let (!label_map, _, !parents_and_children) = genParentsAndChildren labels
    --     !env = Env { --test_results= IM.fromAscList $ zip [0..] test_results,
    --                 -- parents_and_children=parents_and_children,
    --                 -- label_map=label_map
    --                 }
    --     rules = [rTFail, rTPass,
    --             rTFailFreq, rTPassFreq,
    --             rTFailUniqueBranch,
    --             rTFailFreqDiffParent] 

    -- mapM print $ map (\l -> map (\r -> r env l) rules) labels 
    print (length labels)
    print (sum $ map length labels)
    -- print (isSorted IS.empty $ map loc_group labels)
    let total_tests = length test_results
        total_succesful_tests = length $ filter (\(_,b,_) -> b) test_results
        total_failing_tests = total_tests - total_succesful_tests
        env = Env { total_tests=total_tests,
                    total_succesful_tests = total_succesful_tests, 
                    loc_groups = loc_groups}
    print (total_tests, total_failing_tests)
    
    mapM_ (print . (\ls -> ruleOnModule env (relevantTests test_results ls) ls)) labels

    error "Rules run!"

relevantTests :: [((String, String), Bool, IntSet)] -> [Label]
              -> [((String, String), Bool, IntSet)]
relevantTests all_tests labels = filter is_rel all_tests
   where lset = IS.fromAscList $ map loc_index labels
         is_rel (_,_,is) = not (IS.disjoint lset is)

ruleOnModule :: Environment -> [((String, String), Bool, IntSet)] -> [Label] -> [Int]
ruleOnModule env rel_tests module_labels = 
    [length rel_tests, length module_labels]


data Environment = Env {
                      total_tests :: Int,
                      total_succesful_tests :: Int,
                      loc_groups :: IntMap String
                     -- parents_and_children :: !(IntMap ([Int], IntSet)),
                     -- label_map :: !(IntMap Label)
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
rTFailFreq (_) (Label{..}) = 0.0
    
rTPassFreq :: Rule
rTPassFreq (_) (Label{..}) = 0.0

rTFailUniqueBranch :: Rule
rTFailUniqueBranch (_) (Label{..}) = 0.0

rTFailFreqDiffParent :: Rule
rTFailFreqDiffParent (_) (Label{..}) = 0.0

