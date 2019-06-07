{-# LANGUAGE BangPatterns #-}

module STTest
    ( spec
    ) where

import Prelude

import Control.Monad (forM_)
import Test.Hspec (SpecWith, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (property, (===), choose, forAll)

import Task1 (runST, readSTRef, newSTRef, writeSTRef)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib i = runST $ do
  s <- newSTRef 0
  t <- newSTRef 1
  forM_ [2..i] $ \_ -> do
    _s <- readSTRef s
    _t <- readSTRef t
    writeSTRef s _t
    writeSTRef t (_s + _t)
  readSTRef t

eps :: Double
eps = 1.0e-12

whileM :: Monad m => m Bool -> m () -> m ()
whileM c act =
  c >>= \b ->
    if b
      then act >> whileM c act
      else pure ()

sqrt' :: Double -> Double
sqrt' x
  | x < 1 = error "x < 1 not supported"
  | x == 0 = 0
  | otherwise = runST $ do
      l <- newSTRef 0
      r <- newSTRef x
      let checkCond = do
            l_ <- readSTRef l
            r_ <- readSTRef r
            return (r_ - l_ > eps)
      whileM checkCond $ do
        l_ <- readSTRef l -- l^2 < x
        r_ <- readSTRef r -- r^2 >= x
        let m = (l_ + r_) / 2
        if m * m >= x
          then writeSTRef r m
          else writeSTRef l m
      readSTRef r

naiveFib :: Int -> Int
naiveFib n = go n (0,1)
  where
    go !k (!a, !b)
      | k == 0      = a
      | otherwise   = go (k - 1) (b, a + b)

testFib :: SpecWith ()
testFib = describe "tests correctness of i-th fibonacci number calculation" $ do
  it "returns correct 10-th fib number" $
    fib 10 `shouldBe` 55
  it "returns same result with naive implementation" $ property $
    forAll (choose (0, 10000)) $
    \n -> fib n === naiveFib n

testSqrt :: SpecWith ()
testSqrt = describe "tests correctness of sqrt' calculation" $ do
  it "returns correct root of 25" $
    sqrt' 25.0 `shouldSatisfy` (\a -> abs(a - sqrt 25.0) < eps)
  it "returns same result with built-in sqrt" $ property $
    forAll (choose (eps, 100000.0)) $
     \n -> (abs (sqrt' n - sqrt n) < eps) === True

spec :: SpecWith ()
spec = do
  testFib
  testSqrt
