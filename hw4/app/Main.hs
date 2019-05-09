module Main
    ( main
    ) where

import Prelude

import Data.Maybe (fromJust)
import Data.List (foldl1', foldl')
import System.Random (randoms, StdGen, newStdGen)

import Task1 (multiply)

generateMatrix :: Int -> Int -> StdGen -> [[Int]]
generateMatrix n k gen = replicate n $ take k $ randoms gen

sumElement :: [[Int]] -> Int
sumElement matr = let n = length matr
                  in foldl1' (+) $ foldl' (zipWith (+)) (replicate n 0) matr

main :: IO ()
main = do
  gen <- newStdGen
  let l = generateMatrix 10 100000 gen
      r = generateMatrix 100000 10 gen
      c = fromJust $ multiply l r
  putStrLn "sum of elements of result is ="
  print $ sumElement c
