
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.IntMap as IM

import Test.Tasty.Ingredients.Spectrum.ParseCSV
import Test.Tasty.Ingredients.Spectrum.SBFL

import Test.Tasty.Ingredients.Spectrum.Rules
import Test.Tasty.Ingredients.Spectrum.Types

main :: IO ()
main = defaultMainWithIngredients (defaultIngredients) tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Tarantula works" $
     (tarantula <$> parseCSV "test/tarantula.csv") >>=
       checkExpected [0.5,0.5,0.5,0.63,0.0,0.71,0.83,0.0,0.0,0.0,0.0,0.5] . map snd
  , testCase "Dstar 2 works" $
     (dstar 2 <$> parseCSV "test/dstar.csv") >>=
       checkExpected [2.0,2.0,2.29,2.29,0.13,3.0,0.25,1.33,0.0] . map snd
  , testCase "Dstar 3 works" $
     (dstar 3 <$> parseCSV "test/dstar.csv") >>=
       checkExpected [8.0,8.0,9.14,9.14,0.13,9.0,0.25,2.67,0.0] . map snd
  , testCase "Dstar 3 works" $
     (dstar 3 <$> parseCSV "test/dstar.csv") >>=
       checkExpected [8.0,8.0,9.14,9.14,0.13,9.0,0.25,2.67,0.0] . map snd
  , testCase "Complex type parsing works" $ do
        let complex_type = "forall (m :: * -> *) a. Monad m => SlideLayoutsOf (m a) -> m (SlideLayoutsOf a)"
            lbls = [emptyLabel {loc_info = [complex_type]}]
            [rc] = rConstraints emptyEnv IM.empty lbls
            [ra] = rArity emptyEnv IM.empty lbls

        checkExpected [1.0, 1.0] [rc,ra]
  ]
  where checkExpected expected res =
            assertBool ("Result and expected don't match, expected: "
                        <> show expected <> ", got: " <> show res) $
                    all (\(e,r) -> abs (e-r) < 0.1) $ zip expected res
