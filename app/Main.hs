{-# LANGUAGE RecordWildCards #-}
module Main where


import Options.Applicative
import Data.Tree (drawForest)

import Test.Tasty.Ingredients.Spectrum.GenForest
import Test.Tasty.Ingredients.Spectrum.ParseCSV
import Test.Tasty.Ingredients.Spectrum.Types
import Test.Tasty.Ingredients.Spectrum.SBFL

import Data.List (sortOn)


-- | convert Mix files and CSV to a tree.
data Config = Conf {
        target_file :: !FilePath,
        opt_command :: !Command,
        opt_limit :: !Int

    } deriving (Eq, Show)


data Command = Tree
            | Tarantula
            | Ochiai
            | DStar Integer
  deriving (Show, Eq)

config :: Parser Config
config = Conf <$> argument str (metavar "TARGET" <> help "CSV file to use")
             <*> hsubparser (treeCommand <> tarantulaCommand <> ochiaiCommand <> dstarCommand)
             <*> option auto (long "limit" <> value 0
                            <> short 'n' <> metavar "LIMIT" <> help "The number of results to show")
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
              -- <*> strOption (long "hpc-dir" <> metavar "HPC_DIR" <> help "Location of the mix files" <> showDefault <> value ".hpc/")


opts :: ParserInfo Config
opts = info (config <**> helper)
        (fullDesc <>
         progDesc ("Print the tasty spectrum in TARGET to a tree-view.")  <>
         header "spec-csv-to-tree")

-- | This main is running the CSVToForest part only. 
-- For use of the "Ingredient" see the repositories Readme.
main :: IO ()
main = do Conf {..} <- execParser opts
          tr@(test_results, labeled) <- parseCSV target_file
          -- mapM_ print test_results
          let limit = if opt_limit <= 0 then id else take opt_limit
          case opt_command of 
            Tree -> putStrLn $ drawForest $ map (fmap show) 
                             $ limit $ genForest labeled
            alg -> let sf = case alg of
                              Tarantula -> tarantula
                              Ochiai -> ochiai
                              DStar k -> dstar k
                       res = sortOn ((\i -> -i) . snd) $ sf tr
                       ppr ((Label {loc_name=ln, loc_pos=lp}), score) =
                        ln ++ ":" ++ (show lp) ++ " " ++ show score
                   in mapM_ (putStrLn . ppr) $ limit res


