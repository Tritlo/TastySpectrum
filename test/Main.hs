{-# OPTIONS_GHC -fhpc #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Test.Tasty.Ingredients.Spectrum
import Data.List

import Test.Tasty.Ingredients.Spectrum.ParseCSV
import Test.Tasty.Ingredients.Spectrum.SBFL

main :: IO ()
main = defaultMainWithIngredients (testSpectrum:defaultIngredients) tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]


qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^(7 :: Integer) - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1 :: Integer, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1 :: Integer, 2, 3] `compare` [1,2,2] @?= LT
  , testCase "Tarantula works" $
     (tarantula <$> parseCSV "test/tarantula.csv") >>=
       checkExpected [0.5,0.5,0.5,0.63,0.0,0.71,0.83,0.0,0.0,0.0,0.0,0.5] . map snd
  , testCase "Dstar 2 works" $
     (dstar 2 <$> parseCSV "test/dstar.csv") >>=
       checkExpected [2.0,2.0,2.29,2.29,0.13,3.0,0.25,1.33,0.0] . map snd
  , testCase "Dstar 3 works" $
     (dstar 3 <$> parseCSV "test/dstar.csv") >>=
       checkExpected [8.0,8.0,9.14,9.14,0.13,9.0,0.25,2.67,0.0] . map snd
  ]
  where checkExpected expected res = 
            assertBool ("Result and expected don't match, expected: "
                        <> show expected <> ", got: " <> show res) $
                    all (\(e,r) -> abs (e-r) < 0.1) $ zip expected res
