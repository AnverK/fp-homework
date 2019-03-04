module Block1
       ( order3
       , smartReplicate
       , contains
       , stringSum
       ) where

import Prelude

import Data.List (elem, filter, map, replicate, sum, words)
import Text.Read (read)

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z)
  | y < x     = order3(y, x, z)
  | z < y     = order3(x, z, y)
  | otherwise = (x, y, z)

-- for negative numbers it doesn't replicate element
smartReplicate :: [Int] -> [Int]
smartReplicate = foldr (\x -> (++) (replicate x x)) []

contains :: (Eq a) => a -> [[a]] -> [[a]]
contains x = filter (elem x)

stringSum :: String -> Int
stringSum [] = 0
stringSum l  = sum $ map read $ words l
