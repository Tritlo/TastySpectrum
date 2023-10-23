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
        target_file :: FilePath
    } deriving (Eq, Show)


data Action = Tree
            | Tarantula
            | Ochiai
            | Dstar Integer
  deriving (Show, Eq)

config :: Parser Config
config = Conf <$> argument str (metavar "TARGET" <> help "CSV file to use")
              -- <*> strOption (long "hpc-dir" <> metavar "HPC_DIR" <> help "Location of the mix files" <> showDefault <> value ".hpc/")


opts :: ParserInfo Config
opts = info (config <**> helper)
        (fullDesc <>
         progDesc ("Print the tasty spectrum in TARGET to a tree-view.")  <>
         header "spec-csv-to-tree")

main :: IO ()
main = do Conf {..} <- execParser opts
          tr@(test_results, labeled) <- parseCSV target_file
          -- TODO: parse these from flags
          let act = Tarantula
              limit = 10
          -- mapM_ print test_results
          case act of 
            Tree -> putStrLn $ drawForest $ map (fmap show) $ genForest labeled
            alg -> let sf = case alg of
                              Tarantula -> tarantula
                              Ochiai -> ochiai
                              Dstar k -> dstar k
                       res = sortOn ((\i -> -i) . snd) $ sf tr
                       ppr ((Label {loc_name=ln, loc_pos=lp}), score) =
                        ln ++ ":" ++ (show lp) ++ " " ++ show score
                   in mapM_ (putStrLn . ppr) $ take limit res


