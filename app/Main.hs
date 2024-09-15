{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Tree (drawForest)
import Options.Applicative

import Test.Tasty.Ingredients.Spectrum.GenForest
import Test.Tasty.Ingredients.Spectrum.ParseCSV
import Test.Tasty.Ingredients.Spectrum.Rules
import Test.Tasty.Ingredients.Spectrum.SBFL
import Test.Tasty.Ingredients.Spectrum.Types
import Test.Tasty.Ingredients.Spectrum.Weights

import Data.List (isPrefixOf, sortBy, sortOn)
import qualified Data.Map as Map

import qualified Data.IntMap.Strict as IM
import Data.Semigroup ((<>))

-- | convert Mix files and CSV to a tree.
data Config = Conf
    { target_file :: !FilePath
    , opt_command :: !Command
    , opt_limit :: !Int
    , ignore :: !String
    , use_scaling :: !Bool
    , show_leaf_distance :: !Bool
    , show_root_distance :: !Bool
    , json :: !Bool
    , json_out :: !FilePath
    , validate_types :: !Bool
    }
    deriving (Eq, Show)

data Command
    = Tree
    | Rules
    | Weights FilePath
    | Tarantula
    | Ochiai
    | DStar Integer
    | MergeLines
    | Filter String
    deriving (Show, Eq)

config :: Parser Config
config =
    Conf
        <$> argument str (metavar "TARGET" <> help "CSV file to use")
        <*> hsubparser
            ( treeCommand
                <> rulesCommand
                <> weightsCommand
                <> tarantulaCommand
                <> ochiaiCommand
                <> dstarCommand
                <> mergeCommand
                <> filterCommand
            )
        <*> option auto (long "limit" <> value 0 <> short 'n' <> metavar "LIMIT" <> help "The number of results to show")
        <*> strOption (long "ignore" <> value "" <> metavar "IGNORE" <> help "Paths to ignore (e.g. 'tests/Main.hs src/Data/Module/File.hs')")
        <*> switch (long "use-scaling" <> help "Use ratio of failing/passing evals to sort identical scores")
        <*> switch (long "show-leaf-distance" <> help "Display the distance of the expression from a leaf node")
        <*> switch (long "show-root-distance" <> help "Display the distance of the expression from a root node")
        <*> switch (long "json" <> help "Rules JSON output")
        <*> strOption (long "json-out" <> value "" <> metavar "JSONOUT" <> help "File to write JSON output to")
        <*> switch (long "validate-types" <> help "Validate types when running rules.")
  where
    treeCommand :: Mod CommandFields Command
    treeCommand = command "tree" (info (pure Tree) (progDesc "Show a tree of the results"))
    mergeCommand :: Mod CommandFields Command
    mergeCommand = command "merge" (info (pure MergeLines) (progDesc "Merge expressions in the same line in the CSV"))
    rulesCommand :: Mod CommandFields Command
    rulesCommand = command "rules" (info (pure Rules) (progDesc "Use the rules"))
    tarantulaCommand :: Mod CommandFields Command
    tarantulaCommand = command "tarantula" (info (pure Tarantula) (progDesc "Use the tarantula algorithm"))
    ochiaiCommand :: Mod CommandFields Command
    ochiaiCommand = command "ochiai" (info (pure Ochiai) (progDesc "Use the ochiai algorithm"))
    weightsCommand :: Mod CommandFields Command
    weightsCommand = command "weights" (info weightsOpts (progDesc "Use the weights"))

    weightsOpts :: Parser Command
    weightsOpts = Weights <$> argument str (metavar "PARAMETERS" <> help "Parameters for the weights engine")

    dstarCommand :: Mod CommandFields Command
    dstarCommand = command "dstar" (info dstarOpts (progDesc "Use the dstar algorithm with the given k"))
    dstarOpts :: Parser Command
    dstarOpts = DStar <$> argument auto (metavar "K" <> help "The k to use for dstar")

    filterCommand :: Mod CommandFields Command
    filterCommand = command "filter" (info filterOpts (progDesc "Use an expression to filter a spectrum"))
    filterOpts :: Parser Command
    filterOpts = Filter <$> strOption (long "expr" <> metavar "<expr>" <> help "The expression to use to filter")

opts :: ParserInfo Config
opts =
    info
        (config <**> helper)
        ( fullDesc
            <> progDesc "Print the tasty spectrum in TARGET to a tree-view."
            <> header "spec-csv-to-tree"
        )

{- | This main is running the CSVToForest part only.
For use of the "Ingredient" see the repositories Readme.
-}
main :: IO ()
main = do
    Conf{..} <- execParser opts
    case opt_command of
        Weights parameters_file -> runWeights parameters_file target_file
        _ -> do
            tr'@(test_results, loc_groups, labeled') <- parseCSV target_file
            let limit = if opt_limit <= 0 then id else take opt_limit
                labeled =
                    if null ignore
                        then labeled'
                        else
                            let prefixes = map isPrefixOf $ words ignore
                                anyPrefix loc = any ($ loc) prefixes
                             in IM.filter (anyPrefix . (loc_groups IM.!) . loc_group . head) labeled'
                tr = (test_results, loc_groups, labeled)

            case opt_command of
                Tree ->
                    putStrLn $
                        drawForest $
                            map (fmap show) $
                                limit $
                                    genForest $
                                        concat labeled
                Rules -> runRules (validate_types, json, json_out) tr'
                MergeLines -> mergeLines tr'
                Filter expr -> do
                    let (_, mr) = applyRules validate_types tr'
                        fexpr = read expr
                    print expr
                    print fexpr
                    mergeLines $ compileFilterExpr fexpr mr tr'

                alg ->
                    let sf = case alg of
                            Tarantula -> tarantula
                            Ochiai -> ochiai
                            DStar k -> dstar k
                        -- We first sort it so that the ones with a higher ratio
                        -- are first in case of the same score.
                        comp (_, s1, r1) (_, s2, r2) =
                            case compare s1 s2 of
                                LT -> GT
                                GT -> LT
                                EQ -> case compare r1 r2 of
                                    LT -> GT
                                    GT -> LT
                                    EQ -> EQ
                        !res =
                            if use_scaling
                                then
                                    map (\(a, b, _) -> (a, b)) $
                                        sortBy comp $
                                            scaledEvals $
                                                sf tr
                                else sortOn ((\i -> -i) . snd) $ sf tr
                        ppr (Label{loc_group = ln, loc_pos = lp}, score) =
                            (loc_groups IM.! ln) <> ":" <> show (toHpcPos lp) <> " " <> show score
                     in if show_leaf_distance || show_root_distance
                            then
                                let ds =
                                        if show_leaf_distance
                                            then leafDistances $ concat labeled
                                            else rootDistances $ concat labeled
                                    ppr' b@(l, _) =
                                        ppr b <> " " <> show (ds Map.! l)
                                 in mapM_ (putStrLn . ppr') $ limit res
                            else mapM_ (putStrLn . ppr) $ limit res
