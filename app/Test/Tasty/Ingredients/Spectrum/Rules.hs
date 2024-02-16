{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Ingredients.Spectrum.Rules where

import Control.Parallel.Strategies
import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree
import Data.Tree (drawForest)
import Test.Tasty.Ingredients.Spectrum.GenForest
import Test.Tasty.Ingredients.Spectrum.Types

-- | runRules executes all rules and outputs their results to the console.
-- After applying all rules, it terminates the program.
--
-- DevNote: Some of the rules are "computation heavy", so in order to make things work performant
-- the rules are scoped per module (when applicable) to have less memory need and less checks to do.
runRules :: TestResults -> IO ()
runRules tr@(test_results, loc_groups, grouped_labels) = do
  let total_tests = length test_results
      total_succesful_tests = length $ filter (\(_, b, _) -> b) test_results
      total_failing_tests = total_tests - total_succesful_tests
      showPos :: (Int, Int, Int, Int) -> String
      showPos = show . toHpcPos
      showInfo :: [String] -> String
      showInfo = show
      env =
        Env
          { total_tests = total_tests,
            total_succesful_tests = total_succesful_tests,
            loc_groups = loc_groups
          }
      r _ _ = map (\Label {..} -> IM.size loc_evals)
      rules =
        [ ("rTFail", rTFail),
          ("rTPass", rTPass),
          ("rTFailFreq", rTFailFreq),
          ("rTPassFreq", rTPassFreq),
          ("rTFailUniqueBranch", rTFailUniqueBranch),
          ("rJaccard", rJaccard),
          ("rHamming", rHamming),
          ("rOptimal", rOptimal),
          ("rOptimalP", rOptimalP),
          ("rTarantula", rTarantula),
          ("rOchiai", rOchiai),
          ("rDStar 2", rDStar 2),
          ("rDStar 3", rDStar 3),
          ("rRogot1", rRogot1),
          ("rASTLeaf", rASTLeaf),
          ("rTFailFreqDiffParent", rTFailFreqDiffParent)
        ]

      meta_rules =
        [ ("rTarantulaQuantile", rQuantile "rTarantula"),
          ("rOchiaiQuantile", rQuantile "rOchiai"),
          ("rDStar2Quantile", rQuantile "rDStar 2"),
          ("rDStar3Quantile", rQuantile "rDStar 3")
        ]
      -- [2023-12-31]
      -- We run the rules per group (= haskell-module) and then per_rule. This allows us to
      -- stream* the labels into the rules, as far as is allowed,
      -- though we (sadly) need to parse the whole file first due to
      -- how it is laid out.
      results =
        pmap
          ( \ls_mod@(Label {loc_group = lc} : _) ->
              ( loc_groups IM.! lc,
                zip (map (\Label{..} -> (showPos loc_pos,
                                         showInfo loc_info)) ls_mod) $
                  L.transpose $
                    map
                      ( \(_, rule) ->
                          rule env (relevantTests test_results ls_mod) ls_mod
                      )
                      rules
              )
          )
          grouped_labels

      pmap :: NFData b => (a -> b) -> [a] -> [b]
      pmap f = withStrategy (parList rdeepseq) . map f


      rule_names = Map.fromList $ zip (map fst rules ++ map fst meta_rules) [0 ..]
      meta_results =
        L.foldl'
          ( \res (_, meta_rule) ->
              (meta_rule rule_names env) res
          )
          results
          meta_rules

  putStrLn "Rules:"
  mapM_ (putStrLn . \(n, _) -> "  " ++ n) rules
  mapM_ (putStrLn . \(n, _) -> "  " ++ n) meta_rules
  putStrLn ""
  putStrLn "Results:"
  mapM_
    ( putStrLn . \(fn, rule_results) ->
        ( "  " ++ fn ++ ":\n    "
            ++ show rule_results
        )
    )
    meta_results

-- | Returns an IntMap of all tests touched by a list of statements.
-- "Touched" means executed in a failing *or* passing test.
relevantTests ::
  -- | A list of all the tests and the labels they touch
  [((String, String), Bool, IntSet)] ->
  -- | All labels (=expression) for which the touched tests are to be determined
  [Label] ->
  -- | The tests which are touched by any of the labels. The index is reflects the position in the original list of tests.
  IntMap ((String, String), Bool, IntSet)
relevantTests all_tests labels =
  IM.fromAscList $
    filter (is_rel . snd) $
      zip [0 ..] all_tests
  where
    lset = IS.fromAscList $ map loc_index labels
    -- checks for a given Test if a Label is in the executed list (done via Labels' unique loc_index)
    is_rel (_, _, is) = not (IS.disjoint lset is)

-- | An `Environment` holds a set of (global) values and can be pre-calculated.
-- This eases computing some of the rules, mostly the traditional FL formulas (Ochiai, Tarantula, etc.)
data Environment = Env
  { total_tests :: Int,
    total_succesful_tests :: Int,
    loc_groups :: IntMap String
  }

type Rule =
  Environment ->
  IntMap ((String, String), Bool, IntSet) ->
  [Label] ->
  [Double]

type MetaRule =
  Map String Int ->
  Environment ->
  [(FilePath, [((String,String), [Double])])] ->
  [(FilePath, [((String,String), [Double])])]

-- | Number of failing tests this label is involved in
rTFail :: Rule
rTFail _ _ = map rTFail'
  where
    rTFail' Label {..} = fromIntegral $ length (filter (< 0) $ IM.elems loc_evals)

-- Number of passing tests this label is involved in
rTPass :: Rule
rTPass _ _ = map rTPass'
  where
    rTPass' Label {..} = fromIntegral $ length (filter (> 0) $ IM.elems loc_evals)

-- Number of executions in failing tests
rTFailFreq :: Rule
rTFailFreq _ _ = map rTFailFreq'
  where
    rTFailFreq' Label {..} = fromIntegral $ sum (filter (< 0) $ IM.elems loc_evals)

-- Number of executions in passing tests
rTPassFreq :: Rule
rTPassFreq _ _ = map rTPassFreq'
  where
    rTPassFreq' Label {..} = fromIntegral $ sum (filter (> 0) $ IM.elems loc_evals)

--  [2024-01-01] If the test executes a statement that has a "neighbour"
--  (= same parent) in the AST, but the neighbour is not executed, we can
--  consider the test / execution behavior relevant for the structure.
rTFailUniqueBranch :: Rule
rTFailUniqueBranch _ rel_tests mod_labels = map score mod_labels
  where
    ( label_map,
      all_parents_all_children,
      parents_direct_children
      ) = genParentsAndChildren mod_labels
    score Label {..}
      | [] <- ps = 0
      | (p : _) <- ps,
        (_, dc) <- parents_direct_children IM.! p,
        neighs <- IS.toList $ IS.delete loc_index dc,
        -- tests where this label is executed but not
        -- the neighbor
        unique_tests <-
          map
            ( \n ->
                IM.filter
                  ( \(_, _, rel_inds) ->
                      not (n `IS.member` rel_inds)
                  )
                  in_tests
            )
            neighs =
          fromIntegral $ length unique_tests
      where
        (ps, _) = parents_direct_children IM.! loc_index
        in_tests =
          IM.filter
            ( \(_, _, rel_inds) ->
                loc_index `IS.member` rel_inds
            )
            rel_tests

-- [2024-01-01] Gives the distance of the label from a leaf
rASTLeaf :: Rule
rASTLeaf _ _ = map fromIntegral . leafDistanceList

-- | The (global) tarantula score of this expression
-- Global refers to "per-spectrum" instead of the "per-module" values of other rules.
rTarantula :: Rule
rTarantula Env {..} _ = map ttula
  where
    tp = fromIntegral $ total_succesful_tests
    tf = fromIntegral $ total_tests - total_succesful_tests
    ttula :: Label -> Double
    ttula Label {..} = ftf / (ptp + ftf)
      where
        f = fromIntegral $ length (filter (< 0) $ IM.elems loc_evals)
        p = fromIntegral (IM.size loc_evals) - f
        ftf = f / tf
        ptp = p / tp

-- | The (global) ochiai score of this expression
-- Global refers to "per-spectrum" instead of the "per-module" values of other rules.
rOchiai :: Rule
rOchiai Env {..} _ = map ochiai
  where
    tf = fromIntegral $ total_tests - total_succesful_tests
    ochiai :: Label -> Double
    ochiai Label {..} = f / sqrt (tf * (p + f))
      where
        f = fromIntegral $ length (filter (< 0) $ IM.elems loc_evals)
        p = fromIntegral (IM.size loc_evals) - f

-- | The (global) DStar score of this expression, parametrized by k
-- Global refers to "per-spectrum" instead of the "per-module" values of other rules.
rDStar :: Int -> Rule
rDStar k Env {..} _ = map dstar
  where
    tf = fromIntegral $ total_tests - total_succesful_tests
    dstar :: Label -> Double
    dstar Label {..} = (f ^^ k) / ((tf - f) + p)
      where
        f = fromIntegral $ length (filter (< 0) $ IM.elems loc_evals)
        p = fromIntegral (IM.size loc_evals) - f

-- | Jaccard Distance, taken from "A Framework for Improving Fault Localization Effectiveness Based on Fuzzy Expert System"
-- Jaccard Distance might be simple, but is in a different equivalence class as proven by "A Model for Spectra-based Software Diagnosis".
rJaccard :: Rule
rJaccard Env {..} _ = map jaccard
  where
    tf = fromIntegral $ total_tests - total_succesful_tests
    jaccard :: Label -> Double
    jaccard Label {..} = f / tf + p
      where
        f = fromIntegral $ length (filter (< 0) $ IM.elems loc_evals)
        p = fromIntegral (IM.size loc_evals) - f

-- | Hamming Distance, taken from "A Framework for Improving Fault Localization Effectiveness Based on Fuzzy Expert System"
-- Hamming Distance might be simple, but is in a different equivalence class as proven by "A Model for Spectra-based Software Diagnosis".
-- [2024-01-15] Is this a bit redundant if we have the other tests too?
rHamming :: Rule
rHamming Env {..} _ = map hamming
  where
    tf = total_succesful_tests
    hamming :: Label -> Double
    hamming Label {..} = fromIntegral (f + total_succesful_tests - p)
      where
        f = length (filter (< 0) $ IM.elems loc_evals)
        p = (IM.size loc_evals) - f

-- | "Optimal" SBFL as proposed by "A Model for Spectra-based Software Diagnosis".
rOptimal :: Rule
rOptimal Env {..} _ = map optimal
  where
    tf = fromIntegral $ total_tests - total_succesful_tests
    tp = fromIntegral total_succesful_tests
    optimal :: Label -> Double
    optimal Label {..}
      -- If there are non-covered failures, give -1
      | fn > 0 = -1
      -- Otherwise, give "number of passing tests that are not covered"
      | otherwise = pc
      where
        fc = fromIntegral $ length (filter (< 0) $ IM.elems loc_evals)
        pc = fromIntegral (IM.size loc_evals) - fc
        fn = tf - fc

-- | "OptimalP" SBFL as proposed by "A Model for Spectra-based Software Diagnosis".
rOptimalP :: Rule
rOptimalP Env {..} _ = map optimalP
  where
    optimalP :: Label -> Double
    -- "OptimalP" is "number of executed failing tests", minus ("number of passing tests" divided by "number of total tests + 1)
    optimalP Label {..} = fc - (pc / (fromIntegral total_tests))
      where
        fc = fromIntegral $ length (filter (< 0) $ IM.elems loc_evals)
        pc = fromIntegral (IM.size loc_evals) - fc

-- | Rogot1, as per "A Framework for Improving Fault Localization Effectiveness Based on Fuzzy Expert System"
-- Original paper is "A proposed index for measuring agreement in test-retest studies" by Rogot et al.
-- The other Rogot Formulas fall under already existing equivalence classes as per "A Model for Spectra-based Software Diagnosis" and are left out.
rRogot1 :: Rule
rRogot1 Env {..} _ = map rogot1
  where
    tf = fromIntegral $ total_tests - total_succesful_tests
    tp = fromIntegral total_succesful_tests
    rogot1 :: Label -> Double
    rogot1 Label {..} = 0.5 * (fc / (2 * fc + fn + pc) + (pn / (2 * pn + fn + pc)))
      where
        fc = fromIntegral $ length (filter (< 0) $ IM.elems loc_evals)
        pc = fromIntegral (IM.size loc_evals) - fc
        -- The "Failing-Not-Covered" are simply (Total_Failing - Failing_And_Covered). Our Failing_And_Covered is fc
        fn = tf - fc
        pn = pc - tp

-- [2024-01-01] Is the sum the right idea here?

-- ** T-FAIL-FREQ-DIFF-PARENT: Bonus if this statement was executed very

-- different from their parent**. If a test runs the parent statement 50 times,
-- and the given statement 1 time, this is a bit suspicious. If two statements
-- are executed both 41 times they likely follow the same path and are maybe
-- not the relevant faulty statements.
rTFailFreqDiffParent :: Rule
rTFailFreqDiffParent _ rel_tests labels = map (sum . res) labels
  where
    (lmap, _, pdc) = genParentsAndChildren labels
    res :: Label -> [Double]
    res Label {loc_index = li, loc_evals = evs}
      | ([], _) <- pdc IM.! li = [0.0]
      | ((p : _), _) <- pdc IM.! li,
        Just Label {loc_evals = p_evs} <- lmap IM.!? p =
          mapMaybe (score evs p_evs) (IM.keys rel_tests)
      | otherwise = [0.0]
      where
        score evs p_evs test_index =
          do
            e <- fromIntegral <$> evs IM.!? test_index
            pe <- fromIntegral <$> p_evs IM.!? test_index
            return (e / pe)

-- A meta rule. Computes in what quantile a given score for a given rule is.
-- Note that the referenced rule must already exist.
rQuantile :: String -> MetaRule
rQuantile rule_name r_inds _ r = annotate 0 r
  where
    t_ind = r_inds Map.! rule_name
    ws :: [Double]
    ws = r >>= (map ((L.!! t_ind) . snd) . snd)

    ind_ws :: [IntSet]
    ind_ws =
      map (IS.fromList . map fst) $
        L.groupBy ((==) `on` snd) $
          L.sortOn snd $ zip [0 ..] ws

    fi = fromIntegral
    l_iws = fi $ length ind_ws
    quantile :: Int -> Double
    quantile w_i = fi g_i / l_iws
      where
        Just g_i = L.findIndex (w_i `IS.member`) ind_ws
    annotate ::
      Int ->
      [(FilePath, [(a, [Double])])] ->
      [(FilePath, [(a, [Double])])]
    annotate _ [] = []
    annotate !n ((fp, m) : ms) = (fp, m') : (annotate n' ms)
      where
        (!n', !m') = annotate' n m
        annotate' ::
          Int ->
          [(a, [Double])] ->
          (Int, [(a, [Double])])
        annotate' !n [] = (n, [])
        annotate' !n ((s, ds) : ls) =
          let (fn, ls') = annotate' (n + 1) ls
              -- Could be optimized, but we only do it once!
              ds' = ds ++ [quantile n]
           in (fn, (s, ds') : ls')
