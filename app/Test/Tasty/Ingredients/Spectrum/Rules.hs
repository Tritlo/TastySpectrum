{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
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
runRules tr@(test_results, loc_groups, grouped_labels) = do
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
    -- print (length labels)
    -- print (sum $ map length labels)
    -- print (isSorted IS.empty $ map loc_group labels)
    let total_tests = length test_results
        total_succesful_tests = length $ filter (\(_,b,_) -> b) test_results
        total_failing_tests = total_tests - total_succesful_tests
        env = Env { total_tests=total_tests,
                    total_succesful_tests = total_succesful_tests, 
                    loc_groups = loc_groups}
        r _ _ = map (\Label{..} -> IM.size loc_evals)
        rules = [rTFail,
                 rTPass,
                 rTFailFreq,
                 rTPassFreq,
                 rTFailUniqueBranch,
                 rTarantula,
                 rOchiai,
                 rDStar 2,
                 rASTLeaf]
        -- [2023-12-31]
        -- We run them per group and then per_rule. This allows us to
        -- *stream* the labels into the rules, as far as is allowed,
        -- though we (sadly) need to parse the whole file first due to
        -- how it is laid out.
        results = map (\ls_mod@(Label{loc_group=lc}:_) ->
                       (loc_groups IM.! lc,
                        zip (map loc_pos ls_mod) $
                        L.transpose $
                        map (\rule ->
                          rule env (relevantTests test_results ls_mod) ls_mod)
                          rules)) grouped_labels

    mapM_ (putStrLn . \(fn, rule_results) ->
                            (fn ++ ":\n"
                             ++ show rule_results )
                            ) results
    

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
                        -> [Label] -> [Double]



rTFail :: Rule
rTFail _ _ = map rTFail'
  where rTFail' Label {..} = fromIntegral $ length (filter (<0) $ IM.elems loc_evals)

rTPass :: Rule
rTPass _ _ = map rTPass'
  where rTPass' Label {..} = fromIntegral $ length (filter (>0) $ IM.elems loc_evals)

-- Number of executions in failing tests
rTFailFreq :: Rule
rTFailFreq _ _ = map rTFailFreq'
  where rTFailFreq' Label {..} = fromIntegral $ sum (filter (<0) $ IM.elems loc_evals)

-- Number of executions in passing tests
rTPassFreq :: Rule
rTPassFreq _ _ = map rTPassFreq'
  where rTPassFreq' Label {..} = fromIntegral $ sum (filter (>0) $ IM.elems loc_evals)
    

rTFailUniqueBranch :: Rule
rTFailUniqueBranch _ rel_tests mod_labels = map score mod_labels
    where (label_map, all_parents_all_children,
                      parents_direct_children) = genParentsAndChildren mod_labels
          score Label{..} | [] <- ps = 0
                          | (p:_) <- ps,
                            (_, dc) <- parents_direct_children IM.! p,
                            neighs <- IS.toList $ IS.delete loc_index dc,
                            -- tests where this label is executed but not
                            -- the neighbor
                            unique_tests <- map (\n -> 
                                                filter (\(_,_,rel_inds) ->
                                                  not (n `IS.member` rel_inds))
                                            in_tests) neighs 
                            = fromIntegral $ length unique_tests
            where (ps,_) = parents_direct_children IM.! loc_index
                  in_tests = filter (\(_,_,rel_inds) ->
                                      loc_index  `IS.member` rel_inds)
                                    rel_tests
                  


rASTLeaf :: Rule
rASTLeaf _ _ = map fromIntegral . leafDistanceList

rTarantula :: Rule
rTarantula Env{..} _ = map ttula
  where tp = fromIntegral $ total_succesful_tests
        tf = fromIntegral $ total_tests - total_succesful_tests
        ttula :: Label -> Double
        ttula Label{..} = ftf/(ptp + ftf)
          where f = fromIntegral $ length (filter (<0) $ IM.elems loc_evals)
                p = fromIntegral (IM.size loc_evals) - f
                ftf = f/tf
                ptp = p/tp

rOchiai :: Rule
rOchiai Env{..} _ = map ochiai
  where tf = fromIntegral $ total_tests - total_succesful_tests
        ochiai :: Label -> Double
        ochiai Label{..} = f / sqrt (tf *(p+f))
          where f = fromIntegral $ length (filter (<0) $ IM.elems loc_evals)
                p = fromIntegral (IM.size loc_evals) - f

rDStar :: Int ->  Rule
rDStar k Env{..} _ = map dstar
  where tf = fromIntegral $ total_tests - total_succesful_tests
        dstar :: Label -> Double
        dstar Label{..} = (f ^^ k) / ((tf - f) +p)
          where f = fromIntegral $ length (filter (<0) $ IM.elems loc_evals)
                p = fromIntegral (IM.size loc_evals) - f

-- TODO: This one is *per* test, so we need to iterate over rel_tests here.
rTFailFreqDiffParent :: Rule
rTFailFreqDiffParent _ rel_tests = map (const (0/0))
