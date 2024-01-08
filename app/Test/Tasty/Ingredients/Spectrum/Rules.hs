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
import Data.Maybe (isJust, fromMaybe, mapMaybe)

-- | runRules executes all rules and outputs their results to the console. 
-- After applying all rules, it terminates the program.
-- 
-- DevNote: Some of the rules are "computation heavy", so in order to make things work performant
-- the rules are scoped per module (when applicable) to have less memory need and less checks to do. 
runRules :: TestResults -> IO ()
runRules tr@(test_results, loc_groups, grouped_labels) = do
    let total_tests = length test_results
        total_succesful_tests = length $ filter (\(_,b,_) -> b) test_results
        total_failing_tests = total_tests - total_succesful_tests
        showPos :: (Int, Int, Int, Int) -> String
        showPos = show . toHpcPos
        env = Env { total_tests=total_tests,
                    total_succesful_tests = total_succesful_tests, 
                    loc_groups = loc_groups}
        r _ _ = map (\Label{..} -> IM.size loc_evals)
        rules = [( "rTFail", rTFail)
                ,( "rTPass", rTPass)
                ,( "rTFailFreq", rTFailFreq)
                ,( "rTPassFreq", rTPassFreq)
                ,( "rTFailUniqueBranch", rTFailUniqueBranch)
                ,( "rTarantula", rTarantula)
                ,( "rOchiai", rOchiai)
                ,( "rDStar 2", rDStar 2)
                ,( "rDStar 3", rDStar 3)
                ,( "rASTLeaf", rASTLeaf)
                ,( "rTFailFreqDiffParent", rTFailFreqDiffParent)
                ]
        -- [2023-12-31]
        -- We run the rules per group (= haskell-module) and then per_rule. This allows us to
        -- *stream* the labels into the rules, as far as is allowed,
        -- though we (sadly) need to parse the whole file first due to
        -- how it is laid out.
        results = map (\ls_mod@(Label{loc_group=lc}:_) ->
                       (loc_groups IM.! lc,
                        zip (map (showPos . loc_pos) ls_mod) $
                        L.transpose $
                        map (\(_, rule) ->
                          rule env (relevantTests test_results ls_mod) ls_mod)
                          rules)) grouped_labels

    putStrLn "Rules:"
    mapM_ (putStrLn . \(n, _) -> "  " ++ n) rules
    putStrLn ""
    putStrLn "Results:"
    mapM_ (putStrLn . \(fn, rule_results) ->
                            ("  " ++ fn ++ ":\n    "
                             ++ show rule_results )
                            ) results

-- | Returns an IntMap of all tests touched by a list of statements.
-- "Touched" means executed in a failing *or* passing test.
relevantTests :: 
  [((String, String), Bool, IntSet)] -- ^ A list of all the tests and the labels they touch
  -> [Label]                         -- ^ All labels (=expression) for which the touched tests are to be determined
  -> IntMap ((String, String), Bool, IntSet) -- ^ The tests which are touched by any of the labels. The index is reflects the position in the original list of tests.
relevantTests all_tests labels = IM.fromAscList $ 
                                    filter (is_rel . snd) $
                                    zip [0..] all_tests
   where lset = IS.fromAscList $ map loc_index labels
         -- | checks for a given Test if a Label is in the executed list (done via Labels' unique loc_index)
         is_rel (_,_,is) = not (IS.disjoint lset is)


-- | An `Environment` holds a set of (global) values and can be pre-calculated.
-- This eases computing some of the rules, mostly the traditional FL formulas (Ochiai, Tarantula, etc.)
data Environment = Env {
                      total_tests :: Int,
                      total_succesful_tests :: Int,
                      loc_groups :: IntMap String
                   }

type Rule = Environment -> IntMap ((String, String), Bool, IntSet)
                        -> [Label]
                        -> [Double]



-- Number of failing tests this label is involved in
rTFail :: Rule
rTFail _ _ = map rTFail'
  where rTFail' Label {..} = fromIntegral $ length (filter (<0) $ IM.elems loc_evals)

-- Number of passing tests this label is involved in
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
    
--  [2024-01-01] If the test executes a statement that has a "neighbour"
--  (= same parent) in the AST, but the neighbour is not executed, we can
--  consider the test / execution behavior relevant for the structure.
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
                                                IM.filter (\(_,_,rel_inds) ->
                                                  not (n `IS.member` rel_inds))
                                            in_tests) neighs 
                            = fromIntegral $ length unique_tests
            where (ps,_) = parents_direct_children IM.! loc_index
                  in_tests = IM.filter (\(_,_,rel_inds) ->
                                      loc_index  `IS.member` rel_inds)
                                    rel_tests

-- [2024-01-01] Gives the distance of the label from a leaf
rASTLeaf :: Rule
rASTLeaf _ _ = map fromIntegral . leafDistanceList

-- [2024-01-01] The global tarantula score of this expression
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

-- [2024-01-01] The global ochiai score of this expression
rOchiai :: Rule
rOchiai Env{..} _ = map ochiai
  where tf = fromIntegral $ total_tests - total_succesful_tests
        ochiai :: Label -> Double
        ochiai Label{..} = f / sqrt (tf *(p+f))
          where f = fromIntegral $ length (filter (<0) $ IM.elems loc_evals)
                p = fromIntegral (IM.size loc_evals) - f

-- [2024-01-01] The global DStar score of this expression, parametrized by k
rDStar :: Int ->  Rule
rDStar k Env{..} _ = map dstar
  where tf = fromIntegral $ total_tests - total_succesful_tests
        dstar :: Label -> Double
        dstar Label{..} = (f ^^ k) / ((tf - f) +p)
          where f = fromIntegral $ length (filter (<0) $ IM.elems loc_evals)
                p = fromIntegral (IM.size loc_evals) - f

-- [2024-01-01] Is the sum the right idea here?
-- **T-FAIL-FREQ-DIFF-PARENT: Bonus if this statement was executed very
-- different from their parent**. If a test runs the parent statement 50 times,
-- and the given statement 1 time, this is a bit suspicious. If two statements
-- are executed both 41 times they likely follow the same path and are maybe
-- not the relevant faulty statements.
rTFailFreqDiffParent :: Rule
rTFailFreqDiffParent _ rel_tests labels = map (sum . res) labels
  where (lmap,_,pdc) = genParentsAndChildren labels
        res :: Label -> [Double]
        res Label{loc_index=li, loc_evals=evs}
            | ([],_) <- pdc IM.! li = [0.0]
            | ((p:_),_) <- pdc IM.! li,
              Just Label{loc_evals=p_evs} <- lmap IM.!? p =
                mapMaybe (score evs p_evs) (IM.keys rel_tests)
            | otherwise = [0.0]
           where score evs p_evs test_index =
                    do e <- fromIntegral <$> evs IM.!? test_index
                       pe <- fromIntegral <$> p_evs IM.!? test_index
                       return (e/pe)

