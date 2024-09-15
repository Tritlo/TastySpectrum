{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Tasty.Ingredients.Spectrum.Rules where

import Control.Lens (universeOf)
import Control.Parallel.Strategies
import Data.Data
import Data.Data.Lens (uniplate)
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
import Test.Tasty.Ingredients.Spectrum.GenForest
import Test.Tasty.Ingredients.Spectrum.Parse
import Test.Tasty.Ingredients.Spectrum.Types

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL

import GHC

import Control.Monad (when)
import GHC.Generics (Generic)
import GHC.Hs.Type

import Text.ParserCombinators.ReadP as RP

{- | runRules executes all rules and outputs their results to the console.
After applying all rules, it terminates the program.

DevNote: Some of the rules are "computation heavy", so in order to make things work performant
the rules are scoped per module (when applicable) to have less memory need and less checks to do.
-}
allRules :: [(String, Rule)]
allRules =
    [ ("rTFail", rTFail)
    , ("rTPass", rTPass)
    , ("rPropFail", rPropertiesFail)
    , ("rPropPass", rPropertiesPass)
    , ("rUnitFail", rUnitTestFail)
    , ("rUnitPass", countTestTypes filterForUnitTests True)
    , ("rGoldenFail", rGoldenFail)
    , ("rGoldenPass", rGoldenPass)
    , ("rOtherTestFail", rOtherTestsFail)
    , ("rOtherTestPass", rOtherTestsPass)
    , ("rTFailFreq", rTFailFreq)
    , ("rTPassFreq", rTPassFreq)
    , ("rTFailUniqueBranch", rTFailUniqueBranch)
    , ("rJaccard", rJaccard)
    , ("rHamming", rHamming)
    , ("rOptimal", rOptimal)
    , ("rOptimalP", rOptimalP)
    , ("rTarantula", rTarantula)
    , ("rOchiai", rOchiai)
    , ("rDStar2", rDStar 2)
    , ("rDStar3", rDStar 3)
    , ("rRogot1", rRogot1)
    , ("rASTLeaf", rASTLeaf)
    , ("rTFailFreqDiffParent", rTFailFreqDiffParent)
    , ("rDistToFailure", rDistToFailure)
    , ("rIsIdentifier", rIsIdentifier)
    , ("rTypeLength", rTypeLength)
    , ("rTypeArity", rTypeArity)
    , ("rTypeOrder", rTypeOrder)
    , ("rTypeFunArgs", rTypeFunArgs)
    , ("rTypeConstraints", rTypeConstraints)
    , ("rTypePrimitives", rTypePrimitives)
    , ("rTypeSubTypes", rTypeSubTypes)
    , ("rTypeArrows", rTypeArrows)
    ]

metaRules :: [(String, MetaRule)]
metaRules =
    [ ("rTarantulaQuantile", rQuantile "rTarantula")
    , ("rOchiaiQuantile", rQuantile "rOchiai")
    , ("rDStar2Quantile", rQuantile "rDStar2")
    , ("rDStar3Quantile", rQuantile "rDStar3")
    , ("rNumIdFails", rNumIdFails)
    , ("rNumTypeFails", rNumTypeFails)
    , ("rNumSubTypeFails", rNumSubTypeFails)
    ]

allRuleNames :: [String]
allRuleNames = map fst allRules ++ map fst metaRules

data ModResult
    = MR
    { r_loc_group :: Int -- The module this result applies to
    , r_result ::
        [ ( ( Int
            , (Int, Int, Int, Int) -- Position
            , [String] -- "Info (type)"
            )
          , [Double] -- Result for each rule
          )
        ]
    }
    deriving (Generic, NFData)

data FilterVar = FVar String -- name of a rule result
instance Show FilterVar where
    show (FVar s) = s
data FilterComp = FComp FilterVar FilterOp Double

instance Show FilterComp where
    show (FComp fv fop tr) =
        show fv <> " " <> show fop <> " " <> show tr

data FilterOp = FGeq | FLeq | FLt | FGt | FEq

instance Show FilterOp where
    show FGeq = ">="
    show FLeq = "<="
    show FGt = ">"
    show FLt = "<"
    show FEq = "<"

data FilterExpr
    = FAnd FilterExpr FilterExpr
    | FOr FilterExpr FilterExpr
    | FNot FilterExpr
    | FEComp FilterComp

instance Show FilterExpr where
    show (FAnd e1 e2) =
        "(" <> show e1 <> ") && (" <> show e2 <> ")"
    show (FOr e1 e2) =
        "(" <> show e1 <> ") || (" <> show e2 <> ")"
    show (FNot e1) =
        "not (" <> show e1 <> ")"
    show (FEComp e1) =
        show e1

parseFilterExpr :: RP.ReadP FilterExpr
parseFilterExpr = RP.choice [pand, por, pc]
  where
    pand = do
        RP.char '('
        e1 <- parseFilterExpr
        RP.string ") && ("
        e2 <- parseFilterExpr
        char ')'
        return (FAnd e1 e2)
    por = do
        RP.char '('
        e1 <- parseFilterExpr
        RP.string ") || ("
        e2 <- parseFilterExpr
        RP.char ')'
        return (FOr e1 e2)
    pnot = do
        RP.string "not ("
        e1 <- parseFilterExpr
        RP.char ')'
        return (FNot e1)
    pc = do
        rule <- RP.choice (map RP.string allRuleNames)
        RP.char ' '
        op <- pop
        char ' '
        tr <- RP.readS_to_P reads
        return (FEComp (FComp (FVar rule) op tr))
    pop =
        RP.choice
            [ string ">=" >> return FGeq
            , string "<=" >> return FLeq
            , string ">" >> return FGt
            , string "<" >> return FLt
            , string "==" >> return FEq
            ]

compileFilterExpr :: FilterExpr -> [ModResult] -> Spectrum -> Spectrum
compileFilterExpr fexpr =
    let f = compileFilterExpr' fexpr
     in filterRule f
  where
    compileFilterExpr' :: FilterExpr -> (Map String Double -> Bool)
    compileFilterExpr' (FAnd e1 e2) =
        let f1 = compileFilterExpr' e1
            f2 = compileFilterExpr' e2
         in (\rv -> f1 rv && f2 rv)
    compileFilterExpr' (FOr e1 e2) =
        let f1 = compileFilterExpr' e1
            f2 = compileFilterExpr' e2
         in (\rv -> f1 rv || f2 rv)
    compileFilterExpr' (FNot e1) =
        let f1 = compileFilterExpr' e1
         in not . f1
    compileFilterExpr' (FEComp (FComp (FVar rule) fop tr)) =
        \rv ->
            let v = rv Map.! rule
             in case fop of
                    FGeq -> v >= tr
                    FLeq -> v <= tr
                    FGt -> v > tr
                    FLt -> v < tr
                    FEq -> v == tr

filterRule ::
    (Map String Double -> Bool) -> -- condition, can reference variables
    [ModResult] -> -- cached results
    Spectrum ->
    Spectrum
filterRule cond results tr@(test_results, loc_groups, grouped_labels) =
    let grouped_labels' = IM.map (filter (\Label{..} -> loc_index `IS.member` locs_that_match)) grouped_labels
        test_results' = map (\(s, b, inv) -> (s, b, inv `IS.intersection` locs_that_match)) test_results
     in (test_results', loc_groups, grouped_labels')
  where
    simplify MR{..} = map (\((i, _, _), r) -> (i, r)) r_result
    all_rule_names = Map.fromList $ zip allRuleNames [0 :: Int ..]
    thoseThatHold = map fst . filter (\(_, r) -> cond (Map.map (r !!) all_rule_names))
    locs_that_match = IS.fromList $ concatMap (thoseThatHold . simplify) results

applyRules :: Bool -> Spectrum -> (IntMap [Label], [ModResult])
applyRules validate_types tr@(test_results, loc_groups, grouped_labels) =
    let total_tests = length test_results
        total_succesful_tests = length $ filter (\(_, b, _) -> b) test_results
        total_failing_tests = total_tests - total_succesful_tests
        showInfo :: [String] -> String
        showInfo = show
        env =
            Env
                { total_tests = total_tests
                , total_succesful_tests = total_succesful_tests
                , loc_groups = loc_groups
                , validate_types = validate_types
                }
        r _ _ = map (\Label{..} -> IM.size loc_evals)

        -- [2023-12-31]
        -- We run the rules per group (= haskell-module) and then per_rule. This allows us to
        -- stream* the labels into the rules, as far as is allowed,
        -- though we (sadly) need to parse the whole file first due to
        -- how it is laid out.
        results =
            pmap
                ( \ls_mod@(Label{loc_group = lc} : _) ->
                    MR lc
                        $ zip
                            ( map
                                ( \Label{..} ->
                                    ( loc_index
                                    , loc_pos
                                    , loc_info
                                    )
                                )
                                ls_mod
                            )
                        $ L.transpose
                        $ map
                            ( \(_, rule) ->
                                rule env (relevantTests test_results ls_mod) ls_mod
                            )
                            allRules
                )
                $ IM.elems grouped_labels

        pmap :: (NFData b) => (a -> b) -> [a] -> [b]
        pmap f = withStrategy (parList rdeepseq) . map f

        rule_names = Map.fromList $ zip (map fst allRules ++ map fst metaRules) [0 ..]

        -- Inject meta results
        meta_results =
            L.foldl'
                ( \res (_, meta_rule) ->
                    meta_rule rule_names env res
                )
                results
                metaRules
     in (grouped_labels, meta_results)

runRules :: (Bool, Bool, FilePath) -> Spectrum -> IO ()
runRules
    (validate_types, use_json, json_out)
    tr@(test_results, loc_groups, grouped_labels) = do
        let (grouped_labels, results) = applyRules validate_types tr

        let all_invalid = concatMap invalidTypes grouped_labels
        when (validate_types && not (null all_invalid)) $
            do
                mapM_ (putStrLn . \(t, m) -> "`" ++ t ++ "`:" ++ m) all_invalid
                error "Invalid types found, see above ^"
        if use_json
            then do
                let rks = map fst allRules -- ++ map fst meta_rules
                    json_res = Aeson.toJSON $ map construct_kv results
                    construct_kv :: ModResult -> Aeson.Value
                    construct_kv (MR lc res) =
                        Aeson.toJSON $
                            Map.fromList
                                [ ("filename", Aeson.toJSON fn)
                                , ("locations", Aeson.toJSON $ map c res)
                                ]
                      where
                        fn = loc_groups IM.! lc
                        c ((_, loc, info), vals) =
                            Aeson.toJSON $
                                Map.fromList
                                    [ ("location", Aeson.toJSON $ showPos loc)
                                    ,
                                        ( "info"
                                        , Aeson.toJSON $
                                            Map.fromList
                                                [ ("type", ty)
                                                , ("identifier", ident)
                                                ]
                                        )
                                    ,
                                        ( "results"
                                        , Aeson.toJSON $
                                            Map.fromList $
                                                zip rks vals
                                        )
                                    ]
                          where
                            ty
                                | (x : _) <- info = Just x
                                | otherwise = Nothing
                            ident
                                | [_, i] <- info = Just i
                                | otherwise = Nothing
                 in if null json_out
                        then BSL.putStr $ Aeson.encode json_res
                        else Aeson.encodeFile json_out json_res
            else do
                putStrLn "Rules:"
                mapM_ (putStrLn . \(n, _) -> "  " ++ n) allRules
                -- mapM_ (putStrLn . \(n, _) -> "  " ++ n) meta_rules
                putStrLn ""
                putStrLn "Results:"
                mapM_
                    ( putStrLn . \MR{..} ->
                        "  "
                            ++ (loc_groups IM.! r_loc_group)
                            ++ ":\n    "
                            ++ show
                                (map (\((_, l, i), d) -> (showPos l, i, d)) r_result)
                    )
                    results
      where
        showPos :: (Int, Int, Int, Int) -> String
        showPos = show . toHpcPos

data JsonResult = JR
    { jr_location :: String
    , jr_lnfo :: Map String (Maybe String)
    , jr_lesults :: Map String Double
    }

{- | Returns an IntMap of all tests touched by a list of statements.
"Touched" means executed in a failing *or* passing test.
-}
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

-- | Filters an an IntMap of all Properties touched by a list of statements.
filterForProperties :: IntMap ((String, String), Bool, IntSet) -> IntMap ((String, String), Bool, IntSet)
filterForProperties = IM.filter (\((_, t), _, _) -> t == "QC")

-- | Filters an an IntMap of all UnitTests touched by a list of statements.
filterForUnitTests :: IntMap ((String, String), Bool, IntSet) -> IntMap ((String, String), Bool, IntSet)
filterForUnitTests = IM.filter (\((_, t), _, _) -> t == "TestCase")

-- | Filters an an IntMap of all Golden Tests touched by a list of statements.
filterForGoldenTests :: IntMap ((String, String), Bool, IntSet) -> IntMap ((String, String), Bool, IntSet)
filterForGoldenTests = IM.filter (\((_, t), _, _) -> t == "Golden")

-- | Filters an IntMap of all Other (not Unit,Property,Golden) Tests touched by a list of statements.
filterForOtherTests :: IntMap ((String, String), Bool, IntSet) -> IntMap ((String, String), Bool, IntSet)
filterForOtherTests = IM.filter (\((_, t), _, _) -> (t /= "Golden") && (t /= "TestCase") && (t /= "QC"))

{- | An `Environment` holds a set of (global) values and can be pre-calculated.
This eases computing some of the rules, mostly the traditional FL formulas (Ochiai, Tarantula, etc.)
-}
data Environment = Env
    { total_tests :: Int
    , total_succesful_tests :: Int
    , loc_groups :: IntMap String
    , validate_types :: Bool
    }

emptyEnv :: Environment
emptyEnv = Env 0 0 IM.empty False

type Rule =
    Environment ->
    IntMap ((String, String), Bool, IntSet) ->
    [Label] ->
    [Double]

type MetaRule =
    Map String Int ->
    Environment ->
    [ModResult] ->
    [ModResult]

-- [(FilePath, [((String, [String]), [Double])])] ->
-- [(FilePath, [((String, [String]), [Double])])]

-- | Number of failing tests this label is involved in
rTFail :: Rule
rTFail _ _ = map rTFail'
  where
    rTFail' Label{..} = fromIntegral $ length (filter (< 0) $ IM.elems loc_evals)

-- Number of passing tests this label is involved in
rTPass :: Rule
rTPass _ _ = map rTPass'
  where
    rTPass' Label{..} = fromIntegral $ length (filter (> 0) $ IM.elems loc_evals)

-- | Counts passing properties per label
rPropertiesPass :: Rule
rPropertiesPass = countTestTypes filterForProperties True

-- | Counts failing properties per label
rPropertiesFail :: Rule
rPropertiesFail = countTestTypes filterForProperties False

-- | Counts passing Unit Tests per label
rUnitTestPass :: Rule
rUnitTestPass = countTestTypes filterForUnitTests True

-- | Counts failing Unit Tests per label
rUnitTestFail :: Rule
rUnitTestFail = countTestTypes filterForUnitTests False

-- | Counts passing Golden Tests per label
rGoldenPass :: Rule
rGoldenPass = countTestTypes filterForGoldenTests True

-- | Counts failing Golden Tests per label
rGoldenFail :: Rule
rGoldenFail = countTestTypes filterForGoldenTests False

-- | Counts passing Other Tests per label
rOtherTestsPass :: Rule
rOtherTestsPass = countTestTypes filterForOtherTests True

-- | Counts failing Other Tests per label
rOtherTestsFail :: Rule
rOtherTestsFail = countTestTypes filterForOtherTests False

{- | Prototype-Function that looks over the labels and Spectrum
to count how often the labels are in a Sub-Set of the original Spectrum.
-}
countTestTypes ::
    -- | A filter for which tests to use, e.g. only properties
    (IntMap ((String, String), Bool, IntSet) -> IntMap ((String, String), Bool, IntSet)) ->
    -- | Whether or not we want to count failures or passes (TRUE = Pass)
    Bool ->
    -- | A finished Rule, ready to use.
    Rule
countTestTypes filterFunction testStatus _ rel_tests = map countOccurrences
  where
    relevantTests :: IntMap ((String, String), Bool, IntSet)
    relevantTests = IM.filter (\((_, _), b, _) -> b == testStatus) (filterFunction rel_tests)
    touched_labels :: [IntSet]
    touched_labels = [touched | ((_, _), _, touched) <- IM.elems relevantTests]
    countOccurrences :: Label -> Double
    countOccurrences Label{..} = fromIntegral $ countLocations loc_index touched_labels
      where
        countLocations :: Int -> [IntSet] -> Int
        countLocations index labels = length (filter (IS.member index) labels)

-- Number of executions in failing tests
rTFailFreq :: Rule
rTFailFreq _ _ = map rTFailFreq'
  where
    rTFailFreq' Label{..} = fromIntegral $ sum (filter (< 0) $ IM.elems loc_evals)

-- Number of executions in passing tests
rTPassFreq :: Rule
rTPassFreq _ _ = map rTPassFreq'
  where
    rTPassFreq' Label{..} = fromIntegral $ sum (filter (> 0) $ IM.elems loc_evals)

--  [2024-01-01] If the test executes a statement that has a "neighbour"
--  (= same parent) in the AST, but the neighbour is not executed, we can
--  consider the test / execution behavior relevant for the structure.
rTFailUniqueBranch :: Rule
rTFailUniqueBranch _ rel_tests mod_labels = map score mod_labels
  where
    ( label_map
        , all_parents_all_children
        , parents_direct_children
        ) = genParentsAndChildren mod_labels
    score Label{..}
        | [] <- ps = 0
        | (p : _) <- ps
        , (_, dc) <- parents_direct_children IM.! p
        , neighs <- IS.toList $ IS.delete loc_index dc
        , -- tests where this label is executed but not
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

-- [2024-02-17] Type-based rules
--
-- Do we have an identifier?
rIsIdentifier :: Rule
rIsIdentifier _ _ =
    map
        ( fromIntegral
            . ( \Label{..} ->
                    if length loc_info == 2
                        then 1
                        else 0
              )
        )

-- How long is the type, character-wise?
rTypeLength :: Rule
rTypeLength _ _ =
    map
        ( fromIntegral
            . ( \Label{..} ->
                    case loc_info of
                        (x : _) -> length x
                        _ -> 0
              )
        )

-- Type analysis
analyzeType :: (HsType GhcPs -> Double) -> Rule
analyzeType analysis Env{..} _ =
    map
        ( \Label{..} ->
            case loc_info of
                (x : _) | Right t <- parseInfoType x -> analysis t
                (x : _)
                    | Left e <- parseInfoType x ->
                        if validate_types
                            then error $ "Error while parsing `" ++ x ++ "`:" ++ e
                            else -1
                _ -> -1
        )

invalidTypes :: [Label] -> [(String, String)]
invalidTypes =
    mapMaybe
        ( \Label{..} ->
            case loc_info of
                (x : _) | Left e <- parseInfoType x -> Just (x, e)
                _ -> Nothing
        )

-- How many types are there in the type? Int is one, Int -> Int is 3:
-- Int, (->), and Int
rTypeSubTypes :: Rule
rTypeSubTypes = analyzeType (fromIntegral . length . flatTy)
  where
    flatTy = universeOf uniplate

rTypeOrder :: Rule
rTypeOrder = analyzeType (fromIntegral . length . filter isHsAppTy . flatTy)
  where
    isHsAppTy d = toConstr d == toConstr (HsAppTy{} :: HsType GhcPs)
    flatTy = universeOf uniplate

-- | Number of arguments that are parenthesizedx
rTypeFunArgs :: Rule
rTypeFunArgs = analyzeType (fromIntegral . length . filter isHsParTy . flatTy)
  where
    isHsParTy d = toConstr d == toConstr (HsParTy{} :: HsType GhcPs)
    flatTy = universeOf uniplate

-- | Gets the number of constraints a type has.
rTypeConstraints :: Rule
rTypeConstraints = analyzeType (fromIntegral . sum . map ctxtLength . flatTy)
  where
    isHsAppTy d = toConstr d == toConstr (HsAppTy{} :: HsType GhcPs)
    flatTy = universeOf uniplate
    ctxtLength :: HsType GhcPs -> Int

    ctxtLength (HsQualTy{hst_ctxt = ctxt}) = length (unLoc ctxt)
    ctxtLength _ = 0

-- How many arrows are there? Note this is not exactly the arity,
-- since we might have a -> (a -> b) -> c, which has arity 2 but 3 arrows.
rTypeArrows :: Rule
rTypeArrows = analyzeType (fromIntegral . length . filter isHsFunTy . flatTy)
  where
    isHsFunTy d = toConstr d == toConstr (HsFunTy{} :: HsType GhcPs)
    flatTy = universeOf uniplate

-- | Gives the function arity for simple types
rTypeArity :: Rule
rTypeArity = analyzeType (fromIntegral . firstNonZero . map countArgs . flatTy)
  where
    countArgs :: HsType GhcPs -> Int

    countArgs (HsFunTy _ _ _ y) =
        1 + countArgs (unLoc y)
    countArgs (HsParTy _ ty) = 1 + (countArgs $ unLoc ty)
    countArgs _ = 0
    -- We don't do more complex than that.
    flatTy = universeOf uniplate
    firstNonZero [] = 0
    firstNonZero (x : xs) | x == 0 = firstNonZero xs
    firstNonZero (x : _) = x

-- How many concrete types are there? E.g. Int, String, etc.
-- So NumTypesInType [Int] will be 2, the [] and the Int,
-- rTypeArity will be 0 and NumConcreteTypesInType will be 1.
rTypePrimitives :: Rule
rTypePrimitives = analyzeType (fromIntegral . length . filter isHsTyVar . flatTy)
  where
    isHsTyVar d = toConstr d == toConstr (HsTyVar{} :: HsType GhcPs)
    flatTy = universeOf uniplate

{- | The (global) tarantula score of this expression
Global refers to "per-spectrum" instead of the "per-module" values of other rules.
-}
rTarantula :: Rule
rTarantula Env{..} _ = map ttula
  where
    tp = fromIntegral total_succesful_tests
    tf = fromIntegral $ total_tests - total_succesful_tests
    ttula :: Label -> Double
    ttula Label{..} = ftf / (ptp + ftf)
      where
        f = fromIntegral $ length (filter (< 0) $ IM.elems loc_evals)
        p = fromIntegral (IM.size loc_evals) - f
        ftf = f / tf
        ptp = p / tp

{- | The (global) ochiai score of this expression
Global refers to "per-spectrum" instead of the "per-module" values of other rules.
-}
rOchiai :: Rule
rOchiai Env{..} _ = map ochiai
  where
    tf = fromIntegral $ total_tests - total_succesful_tests
    ochiai :: Label -> Double
    ochiai Label{..} = f / sqrt (tf * (p + f))
      where
        f = fromIntegral $ length (filter (< 0) $ IM.elems loc_evals)
        p = fromIntegral (IM.size loc_evals) - f

{- | The (global) DStar score of this expression, parametrized by k
Global refers to "per-spectrum" instead of the "per-module" values of other rules.
-}
rDStar :: Int -> Rule
rDStar k Env{..} _ = map dstar
  where
    tf = fromIntegral $ total_tests - total_succesful_tests
    dstar :: Label -> Double
    dstar Label{..} = (f ^^ k) / ((tf - f) + p)
      where
        f = fromIntegral $ length (filter (< 0) $ IM.elems loc_evals)
        p = fromIntegral (IM.size loc_evals) - f

{- | Jaccard Distance, taken from "A Framework for Improving Fault Localization Effectiveness Based on Fuzzy Expert System"
Jaccard Distance might be simple, but is in a different equivalence class as proven by "A Model for Spectra-based Software Diagnosis".
-}
rJaccard :: Rule
rJaccard Env{..} _ = map jaccard
  where
    tf = fromIntegral $ total_tests - total_succesful_tests
    jaccard :: Label -> Double
    jaccard Label{..} = f / tf + p
      where
        f = fromIntegral $ length (filter (< 0) $ IM.elems loc_evals)
        p = fromIntegral (IM.size loc_evals) - f

{- | Hamming Distance, taken from "A Framework for Improving Fault Localization Effectiveness Based on Fuzzy Expert System"
Hamming Distance might be simple, but is in a different equivalence class as proven by "A Model for Spectra-based Software Diagnosis".
[2024-01-15] Is this a bit redundant if we have the other tests too?
-}
rHamming :: Rule
rHamming Env{..} _ = map hamming
  where
    tf = total_succesful_tests
    hamming :: Label -> Double
    hamming Label{..} = fromIntegral (f + total_succesful_tests - p)
      where
        f = length (filter (< 0) $ IM.elems loc_evals)
        p = IM.size loc_evals - f

-- | "Optimal" SBFL as proposed by "A Model for Spectra-based Software Diagnosis".
rOptimal :: Rule
rOptimal Env{..} _ = map optimal
  where
    tf = fromIntegral $ total_tests - total_succesful_tests
    tp = fromIntegral total_succesful_tests
    optimal :: Label -> Double
    optimal Label{..}
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
rOptimalP Env{..} _ = map optimalP
  where
    optimalP :: Label -> Double
    -- "OptimalP" is "number of executed failing tests", minus ("number of passing tests" divided by "number of total tests + 1)
    optimalP Label{..} = fc - (pc / fromIntegral total_tests)
      where
        fc = fromIntegral $ length (filter (< 0) $ IM.elems loc_evals)
        pc = fromIntegral (IM.size loc_evals) - fc

{- | Rogot1, as per "A Framework for Improving Fault Localization Effectiveness Based on Fuzzy Expert System"
Original paper is "A proposed index for measuring agreement in test-retest studies" by Rogot et al.
The other Rogot Formulas fall under already existing equivalence classes as per "A Model for Spectra-based Software Diagnosis" and are left out.
-}
rRogot1 :: Rule
rRogot1 Env{..} _ = map rogot1
  where
    tf = fromIntegral $ total_tests - total_succesful_tests
    tp = fromIntegral total_succesful_tests
    rogot1 :: Label -> Double
    rogot1 Label{..} = 0.5 * (fc / (2 * fc + fn + pc) + (pn / (2 * pn + fn + pc)))
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
    res Label{loc_index = li, loc_evals = evs}
        | ([], _) <- pdc IM.! li = [0.0]
        | (p : _, _) <- pdc IM.! li
        , Just Label{loc_evals = p_evs} <- lmap IM.!? p =
            mapMaybe (score evs p_evs) (IM.keys rel_tests)
        | otherwise = [0.0]
      where
        score evs p_evs test_index =
            do
                e <- fromIntegral <$> evs IM.!? test_index
                pe <- fromIntegral <$> p_evs IM.!? test_index
                return (e / pe)

-- [2024-02-23]

{- | Calculates how many nodes up and down you have to go in the AST to find a
  failure. I.e. first cousins would be 4, first cousin twice removed would
  be 6 etc. Gives -1 if there is no path
-}
rDistToFailure :: Rule
rDistToFailure env rel_tests labels =
    if null failing_inds
        then replicate (length labels) (fromIntegral (-1))
        else map distToFailing labels
  where
    (_, _, pmap) = genParentsAndChildren labels

    distToFailing :: Label -> Double
    -- 4. For a label
    distToFailing (Label{loc_index = li}) =
        fromIntegral $
            -- If it is failing or a parent of a failing,
            -- it is trivial
            case dists_to_failing_parents IM.!? li of
                Just d -> d
                Nothing ->
                    -- Otherwise, we generate a list of
                    -- distances from this node to its
                    -- parents, and then get the distance
                    let parents = fst (pmap IM.! li)
                     in case go (zip parents [1 ..]) of
                            Just res -> res
                            _ -> (-1)
    -- 3. For a list of parents and distances,
    --    we calculate how far it is from a
    --    failing location
    go ((p, d) : ps) =
        case dists_to_failing_parents IM.!? p of
            Just pd -> Just $ pd + d
            Nothing -> go ps
    go [] = Nothing
    -- 2. For failing indices, we calculate
    --    the distance of each of them to their parents
    dists_to_failing_parents =
        IM.unionsWith min $
            map
                ( \fi ->
                    IM.fromList $
                        (fi, 0) : zip (fst $ pmap IM.! fi) [1 ..]
                )
                failing_inds

    -- 1. We find the failing indices
    failing_inds = mapMaybe has_failure labels
    has_failure Label{..} =
        if any (< 0) $ IM.elems loc_evals
            then Just loc_index
            else Nothing

-- How many times is the identifier, if present, involved in a fault?
rNumIdFails :: MetaRule
rNumIdFails = rNumInfoRule "rTFail" selId (-1) (+)
  where
    selId [_, x] = Just x
    selId _ = Nothing

-- | How many times is the type, if present, involved in a fault?
rNumTypeFails :: MetaRule
rNumTypeFails = rNumInfoRule "rTFail" selTy (-1) (+)
  where
    selTy (x : _) = Just x
    selTy _ = Nothing

rNumSubTypeFails :: MetaRule
rNumSubTypeFails rule_locs Env{..} locs =
    map (\(MR g res) -> MR g (map upd res)) locs
  where
    key = "rTFail"
    key_loc = rule_locs Map.! key
    failing_tys :: Set String
    failing_tys =
        Set.unions $
            concatMap
                ( mapMaybe (get_ty . (\(_, _, inf) -> inf) . fst)
                    . filter isFailing
                    . r_result
                )
                locs
    isFailing (_, vals) = vals L.!! key_loc > 0
    get_ty (x : _) = case parseInfoType x of
        Right t ->
            Just $
                Set.fromList $
                    map showPsType $
                        flatTy t
        _ -> Nothing
    get_ty [] = Nothing
    flatTy = universeOf uniplate

    upd ::
        ((Int, (Int, Int, Int, Int), [String]), [Double]) ->
        ((Int, (Int, Int, Int, Int), [String]), [Double])
    upd ((l, p, inf), vals) = ((l, p, inf), vals ++ [fromIntegral nv])
      where
        nv
            | (ty_str : _) <- inf
            , Right t <- parseInfoType ty_str
            , sub_tys <- map showPsType (flatTy t) =
                length $ filter (`Set.member` failing_tys) sub_tys
            | otherwise = -1

rNumInfoRule ::
    String ->
    ([String] -> Maybe String) ->
    Double ->
    (Double -> Double -> Double) ->
    MetaRule
rNumInfoRule key sel no_val merge rule_locs _ results =
    map (\(MR g res) -> MR g (map upd res)) results
  where
    key_loc = rule_locs Map.! key
    all_res =
        Map.unionsWith merge $
            map
                ( Map.fromListWith merge
                    . mapMaybe f
                    . r_result
                )
                results
    f :: ((Int, (Int, Int, Int, Int), [String]), [Double]) -> Maybe (String, Double)
    f ((_, _, info), vals) = case sel info of
        Just s -> Just (s, vals L.!! key_loc)
        _ -> Nothing
    upd ::
        ((Int, (Int, Int, Int, Int), [String]), [Double]) ->
        ((Int, (Int, Int, Int, Int), [String]), [Double])
    upd ((l, p, inf), vals) = ((l, p, inf), vals ++ [nv])
      where
        nv
            | Just x <- sel inf
            , Just v <- all_res Map.!? x =
                v
            | otherwise = no_val

-- A meta rule. Computes in what quantile a given score for a given rule is.
-- Note that the referenced rule must already exist.
rQuantile :: String -> MetaRule
rQuantile rule_name r_inds _ r = annotate 0 r
  where
    t_ind = r_inds Map.! rule_name
    ws :: [Double]
    ws = r >>= (map ((L.!! t_ind) . snd) . r_result)

    ind_ws :: [IntSet]
    ind_ws =
        map (IS.fromList . map fst) $
            L.groupBy ((==) `on` snd) $
                L.sortOn snd $
                    zip [0 ..] ws

    fi = fromIntegral
    l_iws = fi $ length ind_ws
    quantile :: Int -> Double
    quantile w_i = fi g_i / l_iws
      where
        Just g_i = L.findIndex (w_i `IS.member`) ind_ws
    annotate ::
        Int ->
        [ModResult] ->
        [ModResult]
    -- (FilePath, [(a, [Double])])] ->
    -- [(FilePath, [(a, [Double])])]
    annotate _ [] = []
    annotate !n (MR g m : ms) = MR g m' : annotate n' ms
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
