module GeometryTest
    ( spec
    ) where


import Prelude

import Test.Hspec (SpecWith, describe, it, shouldBe, shouldSatisfy)

import Task2 (Point (..), doubleArea, perimeter)

shortTests :: SpecWith ()
shortTests = describe "short tests" $ do
  let triangle = [Point 0 0, Point 5 0, Point 4 8 ]
  it "returns correct perimeter for general triangle" $
    perimeter triangle `shouldSatisfy` (\p -> abs(p - 22.007) < 0.01)
  it "returns correct area for general triangle" $
    doubleArea triangle `shouldBe` 40
  let quadrangle = [Point 0 0, Point 5 0, Point 4 8, Point 2 7 ]
  it "returns correct perimeter for general triangle" $
    perimeter quadrangle `shouldSatisfy` (\p -> abs(p - 22.578) < 0.01)
  it "returns correct area for general triangle" $
    doubleArea quadrangle `shouldBe` 52

spec :: SpecWith ()
spec = shortTests
