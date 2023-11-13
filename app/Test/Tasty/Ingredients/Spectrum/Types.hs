{-# LANGUAGE RecordWildCards #-}
module Test.Tasty.Ingredients.Spectrum.Types (
        Label (..),
        TestResults,
        TastyTestType (..),
        module Trace.Hpc.Util
            ) where


import Trace.Hpc.Util 

import Data.Function (on)

data Label = Label {loc_name :: String,             -- ^ "Speaking" name of the origin file. Usually something like "source/module/file.hs"
                    loc_pos :: HpcPos,              -- ^ Source Code position of the given mix index, usually translates to something like 4:6-5:11 (from L4 Column 6 to L5 Column 11)
                    loc_index :: Int,               -- ^ The index of the expression in our .csv-file. This is used as a unique identifier.
                    loc_evals :: [(Int,Integer)]}   -- ^ A list of tests and how often this position has been evaluated. 
                                                    --   The first Int is the index of the tests, the second Integer is how often this loc_pos has been evaluated.
                                                    --   The second Integer will be positive for passing tests and negative for failing tests. 
                                                    --   It should not have 0`s as we do not account for non-evaluated things in our data structure. 

-- ! DevNote: 
-- Label is a bit complex, but we try to be performant. 
-- This is why e.g. the Tests are referenced by their index in the tests and not by a String, to save on the comparison.
-- In a similar matter, non-evaluated statements are not included despite them being in the .csv`s. 


data TastyTestType = QuickCheck     -- ^ QuickCheck Property
                    | HUnit         -- ^ HUnit Test Case
                    | Lua           -- ^ Lua Tests 
                    | Golden        -- ^ Golden Tests, comparing Files
                    | TestGroup     -- ^ Unresolved Group, TestTree etc. 
                    | Other         -- ^ Trash Bin Class

instance Show Label where
    show (Label {..}) = 
        loc_name ++ ":" ++ show loc_pos ++ " " ++ show (loc_evals)

instance Eq Label where
  (==) = (==) `on` loc_index

instance Ord Label where
  compare = compare `on` loc_index

-- | The fully parsed TestResult consisting of Locations, Tests and their Execution
type TestResults = (
    [(String,Bool)], -- ^ A list of (Test,TestStatus). True=Passing Test, False=Failing Test
    [Label])          -- ^ The resulting labels and how often they have been executed. The labels also carry the test-status in their loc_evals (see above)

