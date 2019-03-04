module Block2
       ( remove
       , randomIntList
       , mergeSort
       ) where

import Prelude

import Data.List (length, splitAt, take)
import System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

remove :: Int -> [a] -> (Maybe a, [a])
remove _ []       = (Nothing, [])
remove k (x : xs)
  | k < 0     = (Nothing, [])
  | k == 0    = (Just x, xs)
  | otherwise = let (el, l) = remove (k - 1) xs
                in (el, x : l)

mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort list     = let len           = length list
                         (left, right) = splitAt (len `div` 2) list
                     in merge (mergeSort left) (mergeSort right)
  where
    merge :: Ord a => [a] -> [a] -> [a]
    merge [] x = x
    merge x [] = x
    merge l@(x : xs) r@(y : ys)
      | x <= y    = x : merge xs r
      | otherwise = y : merge l ys
