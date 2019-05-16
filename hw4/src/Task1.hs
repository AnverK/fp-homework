-- {-# LANGUAGE BangPatterns #-}

module Task1
    ( multiplyNaive
    , multiply
    ) where

import Prelude

import Control.Monad (guard)
import Data.List (transpose)

import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import qualified Data.Vector as V (Vector, fromList, sum, zipWith)

nonSafeScalarProduct :: [Int] -> [Int] -> Int
nonSafeScalarProduct a b = sum $ zipWith (*) a b

nonSafeScalarProductV :: V.Vector Int -> V.Vector Int -> Int
nonSafeScalarProductV a b = V.sum $ V.zipWith (*) a b

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

  if n < 10000
    then do
      -- `easy` scalar product even on lists
      -- (may be it should be other constant but I think this is sensible)
      let bT = transpose b
          k = length bT
          rowChunkSize = 100000 `div` (n * k)
          -- O(n * k) is complexity of buildRow operation
          buildRowShort row = map (nonSafeScalarProduct row) bT
      if n * k < 10000
        then
          -- too small matrixes to parallelize the computation
          pure (map buildRowShort a)
        else pure (map buildRowShort a `using` parListChunk rowChunkSize rdeepseq)
    else do
      -- for matrixes with high `n` matrixes should be vectorized for performance
      -- purposes. It is enough to vectorize only inner lists for scalar product.
      let aV = map V.fromList a
          bT = map V.fromList (transpose b)
          k = length bT
          rowChunkSize = 100000 `div` (n * k)
      if n < 100000
        then do
          -- n is high but not too much to parallelize the function of mapping
          let buildRowShort row = map (nonSafeScalarProductV row) bT
          pure (map buildRowShort aV `using` parListChunk rowChunkSize rdeepseq)
        else do
          -- in this case, it is sensible to divide building rows on chunks
          let scalarProdChunk = n `div` 100000
              buildRowLong row = map (nonSafeScalarProductV row) bT
                `using` parListChunk scalarProdChunk rdeepseq
          pure (map buildRowLong aV)
