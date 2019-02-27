module Block2
       ( remove
       , randomIntList
       , mergeSort
       ) where

import System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

remove :: Int -> [a] -> Maybe a
remove _ []        = Nothing
remove 0 (x : _) = Just x
remove k (_ : xs) = remove (k - 1) xs

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

