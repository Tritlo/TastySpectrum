{-# OPTIONS_GHC -fno-omit-yields #-}

import Ex
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Spectrum
import Test.Tasty.QuickCheck

main :: IO ()
main =
    defaultMainWithIngredients
        (testSpectrum : defaultIngredients)
        $ testGroup
            "mid"
            [ testProperty "1" $ gcd' 1071 1029 == 21
            , testProperty "2" $ gcd' 0 55 == 55
            ]
