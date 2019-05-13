module GaussTest
    ( spec
    ) where


import Prelude

import Control.Monad (replicateM)
import Data.Maybe (fromJust)
import Test.Hspec (SpecWith, describe, it, shouldBe, shouldNotBe, shouldSatisfy)
import Test.QuickCheck (forAll, (===))
import Test.QuickCheck.Gen (Gen, vectorOf, chooseAny, choose)

import Task3 (gauss, verifySolution)

generateMatrixBySize :: Int -> Gen [[Bool]]
generateMatrixBySize n = replicateM n $ vectorOf n chooseAny

generateListBySize :: Int -> Gen [Bool]
generateListBySize n = vectorOf n chooseAny

generateTestCase :: Gen ([[Bool]], [Bool])
generateTestCase = do
  n <- choose (1, 100)
  a <- generateMatrixBySize n
  b <- generateListBySize n
  return (a, b)

checkAnyAnswer :: [[Bool]] -> [Bool] -> Maybe [Bool] -> Bool
checkAnyAnswer _ _ Nothing    = True
checkAnyAnswer a b (Just ans) = verifySolution a b ans

shortTests :: SpecWith ()
shortTests = describe "short tests" $ do
  let a1 = [[False, False], [False, False]]
      b1 = [False, False]
  it "returns any solution in all-False matrix and vector" $
    gauss a1 b1 `shouldNotBe` Nothing

  let a2 = [[True, False, True], [False, True, True], [True, True, True]]
      b2 = [True, True, False]
      c2 = [True, True, False]
  it "returns the only correct answer if it exists" $
    gauss a2 b2 `shouldBe` Just c2

  let a3 = [[True, False], [True, False]]
      b3 = [True, False]
  it "returns Nothing on system without solution" $
    gauss a3 b3 `shouldBe` Nothing

  let a4 = [[True, False], [True, False]]
      b4 = [True, True]
  it "returns one of possible correct answers if there are more than one solutions" $
    gauss a4 b4 `shouldSatisfy` (verifySolution a4 b4 . fromJust)

generatedTests :: SpecWith ()
generatedTests = describe "generated tests" $
  it "returns correct result for random systems" $ forAll
    generateTestCase (\(a, b) -> checkAnyAnswer a b (gauss a b) === True)

spec :: SpecWith ()
spec = do
  shortTests
  generatedTests
