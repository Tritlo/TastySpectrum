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
        r _ _ = map (\Label{..} -> IM.size loc_evals)
        rules = [rTFail, rTPass,r]
    -- print (total_tests, total_failing_tests)
    
    mapM_ (\rule ->
            mapM_ (print . (\ls_mod -> rule env (relevantTests test_results ls_mod) ls_mod)) labels) rules
    print total_tests
    print total_succesful_tests
    print total_failing_tests
    print "failing:"
    mapM_ print $ filter (\(_,b,_) -> b) test_results

    error "Rules run!"

relevantTests :: [((String, String), Bool, IntSet)] -> [Label]
              -> [((String, String), Bool, IntSet)]
relevantTests all_tests labels = filter is_rel all_tests
   where lset = IS.fromAscList $ map loc_index labels
         is_rel (_,_,is) = not (IS.disjoint lset is)

-- ruleOnModule :: Environment -> [((String, String), Bool, IntSet)] -> [Label] -> [Int]
-- ruleOnModule env rel_tests module_labels = 



data Environment = Env {
                      total_tests :: Int,
                      total_succesful_tests :: Int,
                      loc_groups :: IntMap String
                   }

type Rule = Environment -> [((String, String), Bool, IntSet)]
                        -> [Label] -> [Int]



rTFail :: Rule
rTFail _ _ = map rTFail'
  where rTFail' Label {..} = length (filter (<0) $ IM.elems loc_evals)

rTPass :: Rule
rTPass _ _ = map rTPass'
  where rTPass' Label {..} = length (filter (>0) $ IM.elems loc_evals)


rTFailFreq :: Rule
rTFailFreq _ _ _ = [0]
    
rTPassFreq :: Rule
rTPassFreq _ _ _ = [0]

rTFailUniqueBranch :: Rule
rTFailUniqueBranch _ _ _ = [0]

rTFailFreqDiffParent :: Rule
rTFailFreqDiffParent _ _ _  = [0]

