module Task4
    ( iterateElement
    , fibonacci
    , factorial
    , mapFix
    ) where

import Prelude

import Data.Function (fix)

iterateElement :: a -> [a]
iterateElement = fix helper
  where
    helper :: (a -> [a]) -> a -> [a]
    helper f el = el : f el

fibonacci :: Integer -> Integer
fibonacci = fix helper
  where
    helper :: (Integer -> Integer) -> Integer -> Integer
    helper _ 0 = 1
    helper _ 1 = 1
    helper f n = f (n-1) + f(n-2)

factorial :: Integer -> Integer
factorial = fix helper
  where
    helper :: (Integer -> Integer) -> Integer -> Integer
    helper _ 0 = 1
    helper f n = n * f (n-1)

mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix helper
  where
    helper :: ((a -> b) -> [a] -> [b]) -> ((a -> b) -> [a] -> [b])
    helper _ _ []          = []
    helper prev f (x : xs) = f x : prev f xs
