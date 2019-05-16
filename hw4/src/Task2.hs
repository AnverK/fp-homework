module Task2
    ( Point (..)
    , plus
    , minus
    , scalarProduct
    , crossProduct
    , doubleAreaNaive
    , doubleArea
    , perimeterNaive
    , perimeter
    ) where

import Prelude

import Control.DeepSeq (NFData, rnf)

data Point = Point
  { x :: !Int
  , y :: !Int
  }

instance NFData Point where
  rnf (Point a b) = rnf (a, b)

plus :: Point -> Point -> Point
plus (Point ax ay) (Point bx by) = Point (ax + bx) (ay + by)

minus :: Point -> Point -> Point
minus(Point ax ay) (Point bx by) = Point (ax - bx) (ay - by)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point ax ay) (Point bx by) = ax * bx + ay * by

crossProduct :: Point -> Point -> Int
crossProduct (Point ax ay) (Point bx by) = ax * by - ay * bx

getLength :: Point -> Point -> Double
getLength p1 p2 = let p = minus p1 p2
                  in sqrt $ fromIntegral $ scalarProduct p p

-- one else thing which should be in naive implementation is non-strict fields for Point
perimeterNaive :: [Point] -> Double
perimeterNaive []     = 0
perimeterNaive points = sum (zipWith getLength points (tail $ cycle points))

perimeter :: [Point] -> Double
perimeter []            = 0
perimeter points@(h:xs) = getLength h (last points) +
                          sum (zipWith getLength points xs)

doubleAreaNaive :: [Point] -> Int
doubleAreaNaive []     = 0
doubleAreaNaive points = abs $ sum (zipWith crossProduct points (tail $ cycle points))

doubleArea :: [Point] -> Int
doubleArea []            = 0
doubleArea points@(h:xs) = abs $ crossProduct h (last points) +
                                 sum (zipWith crossProduct points xs)
