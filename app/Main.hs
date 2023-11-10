{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Main where


import Options.Applicative
import Data.Tree (drawForest)

import Test.Tasty.Ingredients.Spectrum.GenForest
import Test.Tasty.Ingredients.Spectrum.ParseCSV
import Test.Tasty.Ingredients.Spectrum.Types
import Test.Tasty.Ingredients.Spectrum.SBFL
import Test.Tasty.Ingredients.Spectrum.Rules

import Data.List (sortOn, isPrefixOf, sortBy)
import qualified Data.Map as Map

import qualified Data.IntMap.Strict as IM


-- | convert Mix files and CSV to a tree.
data Config = Conf {
        target_file :: !FilePath,
        opt_command :: !Command,
        opt_limit :: !Int,
        ignore :: !String,
        use_scaling :: !Bool,
        show_leaf_distance :: !Bool,
        show_root_distance :: !Bool

    } deriving (Eq, Show)


data Command = Tree
            | Tarantula
            | Ochiai
            | DStar Integer
  deriving (Show, Eq)

config :: Parser Config
config = Conf <$> argument str (metavar "TARGET" <> help "CSV file to use")
              <*> hsubparser (treeCommand <> tarantulaCommand <> ochiaiCommand <> dstarCommand)
              <*> option auto (long "limit" <> value 0 <> short 'n' <> metavar "LIMIT" <> help "The number of results to show")
              <*> (strOption (long "ignore" <> value "" <> metavar "IGNORE" <> help "Paths to ignore (e.g. 'tests/Main.hs src/Data/Module/File.hs')"))
              <*> switch (long "use-scaling" <> help "Use ratio of failing/passing evals to sort identical scores")
              <*> switch (long "show-leaf-distance" <> help "Display the distance of the expression from a leaf node")
              <*> switch (long "show-root-distance" <> help "Display the distance of the expression from a root node")
    where 
          treeCommand :: Mod CommandFields Command
          treeCommand = command "tree" (info (pure Tree) (progDesc "Show a tree of the results"))
          tarantulaCommand :: Mod CommandFields Command
          tarantulaCommand = command "tarantula" (info (pure Tarantula) (progDesc "Use the tarantula algorithm"))
          ochiaiCommand :: Mod CommandFields Command
          ochiaiCommand = command "ochiai" (info (pure Ochiai) (progDesc "Use the ochiai algorithm"))
          dstarCommand :: Mod CommandFields Command
          dstarCommand = command "dstar" (info dstarOpts (progDesc "Use the dstar algorithm with the given k"))
          dstarOpts :: Parser Command
          dstarOpts = DStar <$> argument auto (metavar "K" <> help "The k to use for dstar")


opts :: ParserInfo Config
opts = info (config <**> helper)
        (fullDesc <>
         progDesc ("Print the tasty spectrum in TARGET to a tree-view.")  <>
         header "spec-csv-to-tree")

-- | This main is running the CSVToForest part only. 
-- For use of the "Ingredient" see the repositories Readme.
main :: IO ()
main = do Conf {..} <- execParser opts
          print "starting parse..."
          tr'@(test_results, loc_groups, labeled') <- parseCSV target_file
          let limit = if opt_limit <= 0 then id else take opt_limit
              labeled  = if null ignore then labeled'
                         else let prefixes = map isPrefixOf $ words ignore
                                  anyPrefix loc = any ($ loc) prefixes
                              in filter (not . anyPrefix . (loc_groups IM.!) . loc_group) labeled'
              tr = (test_results, loc_groups, labeled)
          runRules tr'
          -- error "done"
          -- case opt_command of 
          --   Tree -> putStrLn $ drawForest $ map (fmap show) 
          --                    $ limit $ genForest labeled
          --   alg -> let sf = case alg of
          --                     Tarantula -> tarantula
          --                     Ochiai -> ochiai
          --                     DStar k -> dstar k
          --              -- We first sort it so that the ones with a higher ratio
          --              -- are first in case of the same score.
          --              comp (_,s1,r1) (_,s2,r2) =
          --                       case compare s1 s2 of
          --                           LT -> GT
          --                           GT -> LT
          --                           EQ -> case compare r1 r2 of
          --                                   LT -> GT
          --                                   GT -> LT
          --                                   EQ -> EQ
          --              !res = if use_scaling
          --                     then map (\(a,b,_) -> (a,b)) $
          --                           sortBy comp $ scaledEvals $ sf tr
          --                     else sortOn ((\i -> -i) . snd) $ sf tr
          --              ppr ((Label {loc_group=ln, loc_pos=lp}), score) =
          --               (loc_groups IM.! ln) <> ":" <> show (toHpcPos lp) <> " " <> show score
                       
          --          in if (show_leaf_distance || show_root_distance)
          --             then let ds =  if show_leaf_distance
          --                            then leafDistances labeled
          --                            else rootDistances labeled
          --                      ppr' b@(l,_) =
          --                           ppr b <> " " <> show (ds Map.! l)
          --                   in mapM_ (putStrLn . ppr') $ limit res
          --             else mapM_ (putStrLn . ppr) $ limit res


