module Task3
    ( gaussNaive
    , verifySolution
    , gauss
    ) where

import Prelude

import Data.List (foldl1', partition)
import qualified Data.Vector as V (Vector, length, map, null, unsafeHead, unsafeIndex,
                                   unsafeTail, unstablePartition, zipWith, (++), fromList)

xor :: Bool -> Bool -> Bool
xor = (/=)

buildLowerTriangleMatrix :: Int -> [[Bool]] -> [[Bool]]
buildLowerTriangleMatrix _ [] = []
buildLowerTriangleMatrix n matr =
  let i = n + 1 - length matr
      (leadingTrue, leadingFalse) = partition (!! i) matr
  in case leadingTrue of
    []     -> head leadingFalse : buildLowerTriangleMatrix n (tail leadingFalse)
    x : xs -> let fixedRows = map (changeLeadingTrue x i) xs
              in x : buildLowerTriangleMatrix n (fixedRows ++ leadingFalse)

changeLeadingTrue :: [Bool] -> Int -> [Bool] -> [Bool]
changeLeadingTrue v i l
  | l !! i    = zipWith xor v l
  | otherwise = l

-- don't forget to reverse the result
bubbleUp :: Int -> [[Bool]] -> Maybe [Bool]
bubbleUp 0    _  = return []
bubbleUp i    matr  = do
  let curRow = last matr
  val <- safeGetValue i curRow
  let fixedRows = map (changeLeadingTrue curRow i) $ init matr
  upperRows <- bubbleUp (i - 1) fixedRows
  return $ val : upperRows

  where
    safeGetValue :: Int -> [Bool] -> Maybe Bool
    safeGetValue k l
      | null l    = Nothing
      | head l    = if l !! k
                      then return True
                      else Nothing
      | otherwise = return False

buildLowerTriangleMatrixVector :: Int -> V.Vector (V.Vector Bool) -> [V.Vector Bool]
buildLowerTriangleMatrixVector n matr
  | V.null matr = []
  | otherwise   =
    let i = n + 1 - V.length matr
        (leadingTrue, leadingFalse) = V.unstablePartition (`V.unsafeIndex` i) matr
    in if V.null leadingTrue
         then
           V.unsafeHead leadingFalse : buildLowerTriangleMatrixVector n (V.unsafeTail leadingFalse)
         else
           let x = V.unsafeHead leadingTrue
               fixedRows = V.map (changeLeadingTrueVector x i) $ V.unsafeTail leadingTrue
           in x : buildLowerTriangleMatrixVector n (fixedRows V.++ leadingFalse)

changeLeadingTrueVector :: V.Vector Bool -> Int -> V.Vector Bool -> V.Vector Bool
changeLeadingTrueVector v i l
  | l `V.unsafeIndex` i    = V.zipWith xor v l
  | otherwise              = l

bubbleUpVector :: Int -> [V.Vector Bool] -> Maybe [Bool]
bubbleUpVector 0    _  = return []
bubbleUpVector i    matr  = do
  let curRow = head matr
  val <- safeGetValueVector i curRow
  let fixedRows = map (changeLeadingTrueVector curRow i) $ tail matr
  downerRows <- bubbleUpVector (i - 1) fixedRows
  return $ val : downerRows

  where
    safeGetValueVector :: Int -> V.Vector Bool -> Maybe Bool
    safeGetValueVector k l
      | V.null l       = Nothing
      | V.unsafeHead l = if l `V.unsafeIndex` k
                      then return True
                      else Nothing
      | otherwise = return False

gaussNaive :: [[Bool]] -> [Bool] -> Maybe [Bool]
gaussNaive a b = do
  let n = length b
      arr = zipWith (:) b a
  res <- bubbleUp n $ buildLowerTriangleMatrix n arr
  return $ reverse res

gauss :: [[Bool]] -> [Bool] -> Maybe [Bool]
gauss a b = do
  let arr = zipWith (:) b a
      vArr = V.fromList $ map V.fromList arr
      n = length vArr
  res <- bubbleUpVector n $ reverse $ buildLowerTriangleMatrixVector n vArr
  return $ reverse res

verifySolution :: [[Bool]] -> [Bool] -> [Bool] -> Bool
verifySolution a b x = let lefts = map calcLeft a
                       in lefts == b
  where
    calcLeft :: [Bool] -> Bool
    calcLeft row = foldl1' xor $ zipWith (&&) x row
