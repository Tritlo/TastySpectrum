import Ex
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Spectrum
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMainWithIngredients (testSpectrum : defaultIngredients) tests

tests :: TestTree
tests =
  testGroup
    "divs"
    [ testCase "17"  $ smallestDiv 13 @?= 13,
      testCase "10"  $ divs 10  @?= [2, 5],
      testCase "15"  $ divs 15  @?= [3, 5],
      testCase "100" $ divs 100 @?= [2, 4, 5, 10, 20, 25, 50],
      testCase "128" $ divs 128 @?= [2, 4, 8, 16, 32, 64],
      testProperty "evens" $ \n -> n > 2 && even n ==> smallestDiv n == 2,
      testProperty "odds" $ \n -> n > 2 && odd n ==> smallestDiv n `mod` n == 0
    ]
