module Lib
       ( plusTwo
       , order3
       , smartReplicate
       , contains
       ) where

plusTwo :: [Int] -> [Int]
plusTwo = map (+2)

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z)
  | y < x     = order3(y, x, z)
  | z < y     = order3(x, z, y)
  | otherwise = (x, y, z)

-- for negative numbers it doesn't replicate element. Integer may be is
-- excessive type but may be someone needs 'almost infinite' list
smartReplicate :: [Integer] -> [Integer]
smartReplicate []       = []
smartReplicate (x : xs) = (myReplicate x x) ++ (smartReplicate xs)
  where
    myReplicate :: Integer -> Integer -> [Integer]
    myReplicate n p
      | n <= 0    = []
      | otherwise = p : (myReplicate (n - 1) p)

contains :: (Foldable t, Eq a) => a -> [t a] -> [t a]
contains x l = filter (elem x) l

