{-# LANGUAGE BangPatterns #-}
module Test.Tasty.Ingredients.Spectrum.SBFL where

import Test.Tasty.Ingredients.Spectrum.Types
import Data.List (partition)

import qualified Data.IntMap.Strict as IM

import Control.Parallel.Strategies

pmap :: NFData b => (a -> b) -> [a] -> [b]
-- pmap f = withStrategy (parList rdeepseq) . map f
-- pmap f = withStrategy (parListChunk chunk_size rdeepseq) . map f
--   where chunk_size = 100000
pmap = map

-- ! DevNote: The Formulas have been extracted from the following Publication:
-- Should I follow this fault localization tool’s output? https://link.springer.com/article/10.1007/s10664-014-9349-1
-- It is a comparative study that lists all formulas in a similar format


totalPassing, totalFailing :: TestResults -> Integer
totalPassing = fst . totalPassFail
totalFailing = snd . totalPassFail

-- | Returns the number of total failing and passing tests for a given spectrum, over all expressions.
totalPassFail :: TestResults -> (Integer, Integer)
totalPassFail (res,_,_) = (toInteger p, toInteger f)
  where p = length $ filter id $ map (\(_,r,_) -> r) res
        f = length res - p

-- | Returns the number of passing and failing tests for a single spectrum-expression
-- TODO: should we be extra save and filter out 0`s ?
passFail ::
      Label                -- ^ A source code location, including the
      -> (Integer,Integer) -- ^ (NumberOfPassing,NumberOfFailing)-Tests
passFail (Label {loc_evals=evals}) = (toInteger p, toInteger f)
    where p = length $ filter (>0) $ IM.elems evals
          f = length evals - p

-- | The Tarantula Formula
-- Relevant Publication: https://dl.acm.org/doi/abs/10.1145/1101908.1101949
tarantula :: TestResults -> [(Label, Double)]
tarantula r@(test_results, _, labeled) = pmap (\l -> (l, ttula l)) $ concat labeled
    where (tp,tf) = totalPassFail r
          ttula label = ftf/(ptp + ftf)
            where (p,f) = passFail label
                  ftf = fromInteger f/fromInteger tf
                  ptp = fromInteger p/fromInteger tp

-- | OCHIAI Formula
-- Original Paper is from Biology and Genetics, so best SE Paper is likely
-- https://link.springer.com/article/10.1007/s10664-014-9349-1
ochiai :: TestResults -> [(Label, Double)]
ochiai r@(test_results, _, labeled) = pmap (\l -> (l, oc l)) $ concat labeled
    where (_,tf) = totalPassFail r
          oc label = fromInteger f/sqrt (fromInteger $ tf*(p+f))
            where (p,f) = passFail label

-- | The DStar Formula
-- Relevant Publication: https://doi.org/10.1109/TR.2013.2285319
dstar :: Integer -> TestResults -> [(Label, Double)]
dstar k r@(test_results, _, labeled)
      | k <= 0 = error "DStar requires k>=1"
      | otherwise = pmap (\l -> (l, ds l)) $ concat labeled
    where (_,tf) = totalPassFail r
          ds label = (fromInteger f^^k)/fromInteger ((tf - f)+p)
            where (p,f) = passFail label


-- Extension based on evals:

passFailEvals ::
  Label                     -- ^ A source code location, including the
  -> ([Integer], [Integer]) -- ^ (EvalsOfPassing,EvalsOfFailing)-Tests
passFailEvals Label{loc_evals=evals} = partition (>0) $ IM.elems evals


scaledEvals:: [(Label,Double)] -> [(Label, Double, Double)]
scaledEvals = map (\(l, d) -> (l, d, scale l))
    -- For each label, we add a number representing the ratio of
    -- failing evals as compared to all evals.
    where scale label = scale
            where (pe,fe) = passFailEvals label
                  !pes = abs $ fromInteger $ sum pe
                  !fes = abs $ fromInteger $ sum fe
                  !scale = fes / (fes + pes)
