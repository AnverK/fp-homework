module MatrixMultiplyTest
    ( spec
    ) where

import Prelude

import Control.Monad (replicateM)
import Data.Maybe (fromJust)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck (forAll, (===))
import Test.QuickCheck.Gen (Gen, vectorOf, choose)
import Data.Matrix (multStd, fromLists)

import Task1 (multiply)

generateMatrixBySize :: Int -> Int -> Gen [[Int]]
generateMatrixBySize n k = replicateM n $ vectorOf k $ choose (-1000, 1000)

generateTestCase :: Gen ([[Int]], [[Int]])
generateTestCase = do
  n <- choose (1, 20)
  k <- choose (1, 20)
  m <- choose (1, 20)
  m1 <- generateMatrixBySize n k
  m2 <- generateMatrixBySize k m
  return (m1, m2)

generateFailTestCase :: Gen ([[Int]], [[Int]])
generateFailTestCase = do
  n <- choose (1, 100)
  k <- choose (1, 50)
  m <- choose (51, 100)
  l <- choose (1, 100)
  m1 <- generateMatrixBySize n k
  m2 <- generateMatrixBySize m l
  return (m1, m2)

shortTests :: SpecWith ()
shortTests = describe "short tests" $ do
  let a1 = [[5, 7], [12, 4]]
      b1 = [[3, 8], [9, 10]]
      c1 = [[78, 110], [72, 136]]
  it "returns correct result on square matrixes" $
    multiply a1 b1 `shouldBe` Just c1
  let a2 = [[2, 9, 1]]
      b2 = [[8, 2], [7, 4], [2, 8]]
      c2 = [[81, 48]]
  it "returns correct matrix on (m,n) and (n,k) matrixes when m > k" $
    multiply a2 b2 `shouldBe` Just c2
  let a3 = [[2], [9], [4]]
      b3 = [[8, 2, 3]]
      c3 = [[16, 4, 6], [72, 18, 27], [32, 8, 12]]
  it "returns correct matrix on (m,n) and (n,k) matrixes when m < k" $
    multiply a3 b3 `shouldBe` Just c3

generatedCorrectTests :: SpecWith ()
generatedCorrectTests = describe "generated tests (correct matrixes)" $
  it "returns correct result on multiplication of random matrixes" $ forAll
    generateTestCase (\(a, b) -> fromLists (fromJust (multiply a b)) ===
                                 multStd (fromLists a) (fromLists b))

generatedUncorrectTests :: SpecWith ()
generatedUncorrectTests = describe "generated tests (uncorrect matrixes)" $
  it "returns Nothing when it's impossible to multiply" $ forAll
    generateFailTestCase (\(a, b) -> multiply a b === Nothing)

spec :: SpecWith ()
spec = do
  shortTests
  generatedCorrectTests
  generatedUncorrectTests
