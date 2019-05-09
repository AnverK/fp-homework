-- {-# LANGUAGE BangPatterns #-}

module Task1
    ( multiplyNaive
    , multiply
    ) where

import Prelude

import Data.List (transpose)
import Control.Monad (guard)

import Control.Parallel.Strategies (rpar, parMap)


nonSafeScalarProduct :: [Int] -> [Int] -> Int
nonSafeScalarProduct a b = sum $ zipWith (*) a b

-- returns multiplication of matrix [m x n] and [l x k] if n == l and returns
-- Nothing otherwise.
multiplyNaive :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplyNaive a b = do
  let n = length $ head a
  guard (n == length b)
  return [ [ nonSafeScalarProduct ax by | by <- transpose b] | ax <- a]

multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply a b = do
  let n = length $ head a
  guard (n == length b)
  let
      bT = transpose b
      buildRow row = parMap rpar (nonSafeScalarProduct row) bT
  pure $ parMap rpar buildRow a
