module Test.Tasty.Ingredients.Spectrum.SBFL where

import Test.Tasty.Ingredients.Spectrum.Types

-- ! DevNote: The Formulas have been extracted from the following Publication:
-- Should I follow this fault localization tool’s output? https://link.springer.com/article/10.1007/s10664-014-9349-1 
-- It is a comparative study that lists all formulas in a similar format


totalPassing, totalFailing :: TestResults -> Integer
totalPassing = fst . totalPassFail
totalFailing = snd . totalPassFail

-- | Returns the number of total failing and passing tests for a given spectrum, over all expressions.
totalPassFail :: TestResults -> (Integer, Integer)
totalPassFail (res,_) = (toInteger p, toInteger f)
  where p = length $ filter id $ map snd res
        f = length res - p

-- | Returns the number of passing and failing tests for a single spectrum-expression
-- TODO: should we be extra save and filter out 0`s ? 
passFail :: 
      Label                -- ^ A source code location, including the 
      -> (Integer,Integer) -- ^ (NumberOfPassing,NumberOfFailing)-Tests
passFail (Label {loc_evals=evals}) = (toInteger p, toInteger f)
    where p = length $ filter (>0) $ map snd evals
          f = length evals - p
    
-- | The Tarantula Formula
-- Relevant Publication: https://dl.acm.org/doi/abs/10.1145/1101908.1101949
tarantula :: TestResults -> [(Label, Double)]
tarantula r@(test_results, labeled) = map (\l -> (l, ttula l)) labeled
    where (tp,tf) = totalPassFail r
          ttula label = ftf/(ptp + ftf)
            where (p,f) = passFail label
                  ftf = (fromInteger f)/(fromInteger tf)
                  ptp = (fromInteger p)/(fromInteger tp)

-- | OCHIAI Formula
-- Original Paper is from Biology and Genetics, so best SE Paper is likely https://link.springer.com/article/10.1007/s10664-014-9349-1
ochiai :: TestResults -> [(Label, Double)]
ochiai r@(test_results, labeled) = map (\l -> (l, oc l)) labeled
    where (_,tf) = totalPassFail r
          oc label = (fromInteger f)/(sqrt $fromInteger $ tf*(p+f))
            where (p,f) = passFail label
-- | The DStar Formula
-- Relevant Publication: https://doi.org/10.1109/TR.2013.2285319
dstar :: Integer -> TestResults -> [(Label, Double)]
dstar k r@(test_results, labeled) 
      | k <= 0 = error "DStar requires k>=1"
      | otherwise = map (\l -> (l, ds l)) labeled
    where (_,tf) = totalPassFail r
          ds label = ((fromInteger f)^^k)/(fromInteger $ (tf - f)+p)
            where (p,f) = passFail label

