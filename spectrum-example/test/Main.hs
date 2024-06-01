{-# OPTIONS_GHC -fno-omit-yields #-}
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Spectrum
import Ex
main :: IO ()
main =
  defaultMainWithIngredients
    (testSpectrum:defaultIngredients) $
  testGroup "mid"
  [ testProperty "1"  $ gcd' 1071 1029 == 21,
    testProperty "2"  $ gcd' 0 55 == 55 ]


