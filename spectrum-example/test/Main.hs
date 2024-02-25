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
  [ testCase "3 3 5" $ mid 3 3 5 @?= 3,
    testCase "1 2 3" $ mid 1 2 3 @?= 2,
    testCase "5 5 5" $ mid 5 5 5 @?= 5,
    testCase "5 3 4" $ mid 5 3 4 @?= 4,
    testCase "2 1 3" $ mid 2 1 3 @?= 2,
    testProperty "geo" $ \x y z ->
    mid x y z == x+y+z - min x (min y z)
                       - max x (max y z)]

