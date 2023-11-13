{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}
module Test.Tasty.Ingredients.Spectrum.Types (
        Label (..),
        TestResults,
        pprLabel,
        module Trace.Hpc.Util
            ) where


import Trace.Hpc.Util

import Data.Function (on)
import Control.Parallel.Strategies (NFData)
import GHC.Generics (Generic)

import Data.IntMap.Strict as IM


data Label = Label { loc_group :: !Int,               -- ^ Index of the module this label belongs to.
                     loc_pos :: !(Int,Int,Int,Int),   -- ^ Source Code position of the given mix index, usually translates to something like 4:6-5:11 (from L4 Column 6 to L5 Column 11)
                     loc_index :: !Int,               -- ^ The index of the expression in our .csv-file. This is used as a unique identifier.
                     loc_evals :: !(IntMap Integer)   -- ^ A list of tests and how often this position has been evaluated
                                                      --   The first Int is the index of the tests, the second Integer is how often this loc_pos has been evaluated.
                                                      --   The second Integer will be positive for passing tests and negative for failing tests.
                                                      --   It should not have 0`s as we do not account for non-evaluated things in our data structure.
                   } deriving (Generic, NFData)

-- ! DevNote:
-- Label is a bit complex, but we try to be performant.
-- This is why e.g. the Tests are referenced by their index in the tests and not by a String, to save on the comparison.
-- In a similar matter, non-evaluated statements are not included despite them being in the .csv`s.
--

pprLabel :: IM.IntMap String -> Label -> String
pprLabel loc_groups Label{..} =
    fn <> ":" <> p <> " " <> show loc_evals
   where fn = loc_groups IM.! loc_group
         p = show $ toHpcPos loc_pos


instance Show Label where
    show (Label {..}) =
        show loc_group ++ "-" ++ show loc_index ++ ":" ++ show loc_pos ++ " " ++ show (loc_evals)

instance Eq Label where
  (==) = (==) `on` loc_index

instance Ord Label where
  compare = compare `on` loc_index


-- | The fully parsed TestResult consisting of Locations, Tests and their Execution
type TestResults = (
    [((String, String), Bool)], -- ^ A list of (TestName,TestType, TestStatus). True=Passing Test, False=Failing Test
    IntMap String,  -- ^ A map that gives the filename of each group of locations
    [Label])          -- ^ The resulting labels and how often they have been executed. The labels also carry the test-status in their loc_evals (see above)

