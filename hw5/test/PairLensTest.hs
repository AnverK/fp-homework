{-# LANGUAGE ScopedTypeVariables #-}

module PairLensTest
    ( spec
    ) where

import Prelude

import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck (property, (===))

import Task5 (_1, _2, set, over, view)

testView :: SpecWith ()
testView = describe "tests pair lenses for view" $ do
  it "returns correct elements of pair after initialization" $
    let p = ([1, 2] :: [Integer], 1337 :: Integer)
    in (view _1 p, view _2 p) `shouldBe` p
  it "returns correct elements of pair after initialization (property)" $ property $
    \p@(_ :: Integer, _ :: String) -> (view _1 p, view _2 p) === p

testSet :: SpecWith ()
testSet = describe "tests pair lenses for set" $ do
  it "returns correct elements of pair after setting value" $
    let p1 = ([1, 2] :: [Integer], 1337 :: Integer)
        p2 = set _1 [5] p1
        p3 = set _2 1 p2
    in p3 `shouldBe` ([5] :: [Integer], 1 :: Integer)
  it "returns correct elements of pair after setting value (property)" $ property $
    \p1@(_ :: Integer, _ :: String) (a :: Integer) (b :: String) ->
      let p2 = set _1 a p1
          p3 = set _2 b p2
      in p3 === (a, b)

testOver :: SpecWith ()
testOver = describe "tests pair lenses for over" $ do
  it "returns correct elements of pair after applying function to value" $
    let p1 = ([] :: [Integer], 1337 :: Integer)
        p2 = over _1 (1 : ) p1
        p3 = over _2 (+1) p2
    in p3 `shouldBe` ([1], 1338)
  it "returns correct elements of pair after applying function to value (property)" $
    property $
      \p1@(x :: Integer, y :: String) (a :: Integer) (b :: String) ->
        let p2 = over _1 (+a) p1
            p3 = over _2 (++ b) p2
        in p3 === (x + a, y ++ b)

testLaws :: SpecWith ()
testLaws = describe "checks lens laws" $ do
  it "checks 1 law: `You get back what you put in`" $ property $
    \p@(_ :: String, _ :: Integer) (a :: String) (b :: Integer) ->
      (view _1 (set _1 a p), view _2 (set _2 b p)) === (a, b)
  it "checks 2 law: `Putting back what you got doesn't change anything`" $ property $
    \p@(_ :: String, _ :: Integer) ->
      (view _1 $ set _1 (view _1 p) p, view _2 $ set _2 (view _2 p) p) === p
  it "checks 3 law: `Setting twice is the same as setting once`" $ property $
    \p@(_ :: String, _ :: Integer) (a :: String) (b :: Integer) ->
      (view _1 $ set _1 a $ set _1 a p, view _2 $ set _2 b $ set _2 b p) === (a, b)

spec :: SpecWith ()
spec = do
  testView
  testSet
  testOver
  testLaws
