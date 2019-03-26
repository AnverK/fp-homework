module StringSumTest
  ( spec
  ) where

import Prelude

import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck (property)

import Block1 (stringSum)

spec :: SpecWith ()
spec = describe "stringSum" $ do
  it "returns correct sum of digits" $
    stringSum "0 1 2 3 4" `shouldBe` Just 10

  it "returns correct sum of positive numbers" $
    stringSum "227 228 229 2210 " `shouldBe` Just 2894

  it "returns correct sum of both positve and negative numbers" $
    stringSum "-20 30 -40 " `shouldBe` Just (-30)

  it "returns correct sum with negative overflow" $
    let x = intListToStr [minBound :: Int, -1]
    in
    stringSum x `shouldBe` Just (maxBound :: Int)

  it "returns correct sum with positve overflow" $
    let x = intListToStr [maxBound :: Int, 1]
    in
    stringSum x `shouldBe` Just (minBound :: Int)

-- probably it should return Nothing here in overflow --
-- but it was not mentioned --
  it "returns correct sum with overflow in number" $
    let x = show (minBound :: Int)
    in
    stringSum (tail x) `shouldBe` Just (minBound :: Int)

  it "returns correct sum with many whitespaces" $
    stringSum "0    1      32      128" `shouldBe` Just 161

  it "return Nothing for string with not acceptable symbols" $
    stringSum "a1 b2" `shouldBe` Nothing

  it "return correct sum string from numbers of type int" $ property $
    \x -> stringSum (unwords (map show x)) == Just (sum x)

  where
    intListToStr l = unwords (map show l)
