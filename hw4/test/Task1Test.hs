module Task1Test
    ( spec
    ) where

import Prelude

import Criterion.Main (Benchmark, defaultMain, bench, nf, env, bgroup)
import System.Random (randoms, StdGen, newStdGen)

import Task1 (multiplyNaive, multiply)

generateMatrix :: Int -> Int -> StdGen -> [[Int]]
generateMatrix n k gen = replicate n $ take k $ randoms gen

setupEnv :: Int -> Int -> Int -> StdGen -> IO ([[Int]], [[Int]])
setupEnv m n k gen = return (generateMatrix m n gen, generateMatrix n k gen)

spec :: IO ()
spec = do
  gen <- newStdGen
  defaultMain [
      bgroup "multiplyNaive" [
          multiplyBench 10 10 10 gen multiplyNaive
        , multiplyBench 50 50 50 gen multiplyNaive
        , multiplyBench 100 100 100 gen multiplyNaive
        , multiplyBench 500 500 500 gen multiplyNaive
        , multiplyBench 10 100000 10 gen multiplyNaive
        , multiplyBench 1000 10 1000 gen multiplyNaive
      ]
    , bgroup "multiply" [
          multiplyBench 10 10 10 gen multiply
        , multiplyBench 50 50 50 gen multiply
        , multiplyBench 100 100 100 gen multiply
        , multiplyBench 500 500 500 gen multiply
        , multiplyBench 10 100000 10 gen multiply
        , multiplyBench 1000 10 1000 gen multiply
      ]
    ]

  where
    multiplyBench :: Int -> Int -> Int -> StdGen -> ([[Int]] -> [[Int]] -> Maybe [[Int]]) -> Benchmark
    multiplyBench m n k gen multiplyMatrix = env (setupEnv m n k gen) $ \ ~(l, r) ->
      bench (show (m, n, k)) $ nf (uncurry multiplyMatrix) (l, r)
