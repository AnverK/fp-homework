module Main
  ( main
  ) where

import Prelude

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, nf)
import System.Random (StdGen, newStdGen, randoms)

import Task1 (multiply, multiplyNaive)
import Task2 (Point (..), doubleArea, doubleAreaNaive, perimeter, perimeterNaive)
import Task3 (gauss, gaussNaive)

main :: IO ()
main = do
  gen <- newStdGen
  defaultMain
    [ bgroup "multiplyNaive"
        [ multiplyBench 50 50 50 gen multiplyNaive
        , multiplyBench 100 100 100 gen multiplyNaive
        , multiplyBench 500 500 500 gen multiplyNaive
        , multiplyBench 10 100000 10 gen multiplyNaive
        , multiplyBench 1000 10 1000 gen multiplyNaive
        ]
    , bgroup "multiply"
        [ multiplyBench 50 50 50 gen multiply
        , multiplyBench 100 100 100 gen multiply
        , multiplyBench 500 500 500 gen multiply
        , multiplyBench 10 100000 10 gen multiply
        , multiplyBench 1000 10 1000 gen multiply
        ]
    , bgroup "perimeterNaive"
        [ perimeterBench 10000 gen perimeterNaive
        , perimeterBench 1000000 gen perimeterNaive
        , perimeterBench 10000000 gen perimeterNaive
        ]
    , bgroup "perimeter"
        [ perimeterBench 10000 gen perimeter
        , perimeterBench 1000000 gen perimeter
        , perimeterBench 10000000 gen perimeter
        ]
    , bgroup "doubleAreaNaive"
        [ areaBench 10000 gen doubleAreaNaive
        , areaBench 1000000 gen doubleAreaNaive
        , areaBench 10000000 gen doubleAreaNaive
        ]
    , bgroup "doubleArea"
        [ areaBench 10000 gen doubleArea
        , areaBench 1000000 gen doubleArea
        , areaBench 10000000 gen doubleArea
        ]
    , bgroup "gaussNaive"
        [ gaussBench 10 gen gaussNaive
        , gaussBench 50 gen gaussNaive
        , gaussBench 100 gen gaussNaive
        , gaussBench 500 gen gaussNaive
        , gaussBench 1000 gen gaussNaive
        ]
    , bgroup "gauss"
        [ gaussBench 10 gen gauss
        , gaussBench 50 gen gauss
        , gaussBench 100 gen gauss
        , gaussBench 500 gen gauss
        , gaussBench 5000 gen gauss
        ]
    ]

  where
    generateMatrix :: Int -> Int -> StdGen -> [[Int]]
    generateMatrix a b gen = replicate a $ take b $ randoms gen

    generateMatrixBool :: Int -> Int -> StdGen -> [[Bool]]
    generateMatrixBool a b _ = replicate a $ replicate b False

    generateListBool :: Int -> StdGen -> [Bool]
    generateListBool n _ = replicate n False

    generatePoint :: StdGen -> Point
    generatePoint gen = let [a, b] = take 2 $ randoms gen
                        in Point a b

    multiplyBench :: Int -> Int -> Int -> StdGen -> ([[Int]] -> [[Int]] -> Maybe [[Int]]) -> Benchmark
    multiplyBench m n k gen multiplyMatrix = env setupEnv $ \ ~(l, r) ->
      bench (show (m, n, k)) $ nf (uncurry multiplyMatrix) (l, r)

      where
        setupEnv :: IO ([[Int]], [[Int]])
        setupEnv = return (generateMatrix m n gen, generateMatrix n k gen)

    areaBench :: Int -> StdGen -> ([Point] -> Int) -> Benchmark
    areaBench n gen calcArea = env setupEnv $ \ points ->
      bench (show n) $ nf calcArea points

      where
        setupEnv :: IO [Point]
        setupEnv = return $ replicate n $ generatePoint gen

    perimeterBench :: Int -> StdGen -> ([Point] -> Double) -> Benchmark
    perimeterBench n gen calcPerimeter = env setupEnv $ \ points ->
      bench (show n) $ nf calcPerimeter points

      where
        setupEnv :: IO [Point]
        setupEnv = return $ replicate n $ generatePoint gen

    gaussBench :: Int -> StdGen -> ([[Bool]] -> [Bool] -> Maybe [Bool]) -> Benchmark
    gaussBench n gen calcGauss = env setupEnv $ \ ~(a, b) ->
      bench (show n) $ nf (uncurry calcGauss) (a, b)

      where
        setupEnv :: IO ([[Bool]], [Bool])
        setupEnv = return (generateMatrixBool n n gen, generateListBool n gen)
