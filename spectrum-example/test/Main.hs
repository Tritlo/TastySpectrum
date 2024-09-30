import Ex
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Spectrum
import Test.Tasty.QuickCheck
import Data.Foldable (traverse_)

main :: IO ()
main = defaultMainWithIngredients (testSpectrum : defaultIngredients) tests

tests :: TestTree
tests =
  testGroup
    "divs"
    [ testGroup
        "divs"
        [ testCase "10"  $ divs 10  `has` [2, 5],
          testCase "15"  $ divs 15  `has` [3, 5],
          testCase "128" $ divs 128 `has` [2, 4, 8, 16, 32, 64]
        ],
      testGroup
        "smallestDiv"
        [ testCase "8" $ smallestDiv 8 @?= 2,
          testCase "13" $ smallestDiv 13 @?= 13,
          testProperty "evens" $
            \n -> n > 2 && even n ==>
              smallestDiv n == 2,
          testProperty "isdiv" $
            \n -> n > 2 ==>
              n `mod` smallestDiv n == 0

        ]
    ]
  where has :: [Int] -> [Int] -> Assertion
        has li els = all (`elem` li) els @? show els <> " not found in " <> show li
