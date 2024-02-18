{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}
module Test.Tasty.Ingredients.Spectrum.Types (
        Label (..),
        emptyLabel,
        TestResults,
        pprLabel,
        pprLabelOnly,
        module Trace.Hpc.Util
            ) where


import Trace.Hpc.Util

import Data.Function (on)
import Control.Parallel.Strategies (NFData)
import GHC.Generics (Generic)

import Data.IntMap.Strict as IM
import Data.IntSet (IntSet)

import Data.Semigroup((<>))

-- | Label is our data type to represent information about a single expression.
-- To help with performance, we avoided a real HPC Position in favour of a loc_pos tuple that can be better handled by many libraries.
-- ! DevNote:
-- Label is a bit complex, but we try to be performant.
-- This is why e.g. the Tests are referenced by their index in the tests and not by a String, to save on the comparison.
-- In a similar matter, non-evaluated statements are not included despite them being in the .csv`s.
data Label = Label { loc_group :: !Int,               -- ^ Index of the module this label belongs to.
                     loc_pos :: !(Int,Int,Int,Int),   -- ^ Source Code position of the given mix index, usually translates to something like 4:6-5:11 (from L4 Column 6 to L5 Column 11)
                     loc_index :: !Int,               -- ^ The index of the expression in our .csv-file. This is used as a unique identifier.
                     loc_info :: ![String],           -- ^ A list of types then identifiers associated with the loc
                     loc_evals :: !(IntMap Integer)   -- ^ A list of tests and how often this position has been evaluated
                                                      --   The first Int is the index of the tests, the second Integer is how often this loc_pos has been evaluated.
                                                      --   The second Integer will be positive for passing tests and negative for failing tests.
                                                      --   It should not have 0`s as we do not account for non-evaluated things in our data structure.
                   } deriving (Generic, NFData)

-- An empty label for testing
emptyLabel :: Label
emptyLabel = Label 0 (0,0,0,0) 0 [] IM.empty

-- | Pretty prints (ppr) a label to a human readable HPC Position.
pprLabel ::
  IM.IntMap String -- ^ an IntMap of the Module ID's and their name
  -> Label         -- ^ the label to be pretty printed
  -> String        -- ^ Pretty printed, human readable output matching an HPC Position
pprLabel loc_groups l@Label{..} =
    pprLabelOnly loc_groups l <> " " <> show loc_evals

pprLabelOnly ::
  IM.IntMap String -- ^ an IntMap of the Module ID's and their name
  -> Label         -- ^ the label to be pretty printed
  -> String        -- ^ Pretty printed, human readable output matching an HPC Position
pprLabelOnly loc_groups Label {..} = fn <> ":" <> p
   where fn = loc_groups IM.! loc_group
         p = show $ toHpcPos loc_pos



instance Show Label where
    show (Label {..}) =
        show loc_group ++ "-" ++
        show loc_index ++ ":" ++
        show loc_pos   ++ " :: " ++
        show loc_info  ++ " " ++
        show (loc_evals)

instance Eq Label where
  (==) = (==) `on` loc_index

instance Ord Label where
  compare = compare `on` loc_index


-- | The fully parsed TestResult consisting of Locations, Tests and their Execution
type TestResults = (
    [((String, String), Bool, IntSet)],
                -- ^ A list of ((TestName,TestType), TestStatus, InvolvedLabels).
                --  True=Passing Test, False=Failing Test
    IntMap String,  -- ^ A map that gives the filename of each group of locations
    [[Label]])          -- ^ The resulting labels and how often they have been executed. The labels also carry the test-status in their loc_evals (see above)

