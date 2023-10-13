{-# LANGUAGE RecordWildCards #-}
module Main where



import Options.Applicative
import Data.Tree (drawForest)

import Test.Tasty.Ingredients.Spectrum.CSVToForest

-- | convert Mix files and CSV to a tree.
data Config = Conf {
        target_file :: FilePath
    } deriving (Eq, Show)


config :: Parser Config
config = Conf <$> argument str (metavar "TARGET" <> help "CSV file to convert to a tree")
              -- <*> strOption (long "hpc-dir" <> metavar "HPC_DIR" <> help "Location of the mix files" <> showDefault <> value ".hpc/")


opts :: ParserInfo Config
opts = info (config <**> helper)
        (fullDesc <>
         progDesc ("Print the tasty spectrum in TARGET to a tree-view.")  <>
         header "spec-csv-to-tree")

main :: IO ()
main = do Conf {..} <- execParser opts
          (test_results, forest) <- csvToForest target_file
          putStrLn $ drawForest $ map (fmap show) forest

