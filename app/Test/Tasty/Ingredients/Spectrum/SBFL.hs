module Test.Tasty.Ingredients.Spectrum.SBFL where


import Test.Tasty.Ingredients.Spectrum.Types


totalPassing, totalFailing :: TestResults -> Integer
totalPassing = fst . totalPassFail
totalFailing = snd . totalPassFail


totalPassFail :: TestResults -> (Integer, Integer)
totalPassFail (res,_) = (toInteger p, toInteger f)
  where p = length $ filter id $ map snd res
        f = length res - p


passFail :: Label -> (Integer,Integer)
passFail (Label {loc_evals=evals}) = (toInteger p, toInteger f)
    where p = length $ filter (>0) $ map snd evals
          f = length evals - p
    

tarantula :: TestResults -> [(Label, Double)]
tarantula r@(test_results, labeled) = map (\l -> (l, ttula l)) labeled
    where (tp,tf) = totalPassFail r
          ttula label = ftf/(ptp + ftf)
            where (p,f) = passFail label
                  ftf = (fromInteger f)/(fromInteger tf)
                  ptp = (fromInteger p)/(fromInteger tp)

ochiai :: TestResults -> [(Label, Double)]
ochiai r@(test_results, labeled) = map (\l -> (l, oc l)) labeled
    where (_,tf) = totalPassFail r
          oc label = (fromInteger f)/(sqrt $fromInteger $ tf*(p+f))
            where (p,f) = passFail label

dstar :: Integer -> TestResults -> [(Label, Double)]
dstar k r@(test_results, labeled) = map (\l -> (l, ds l)) labeled
    where (_,tf) = totalPassFail r
          ds label = ((fromInteger f)^^k)/(fromInteger $ (tf - f)+p)
            where (p,f) = passFail label

