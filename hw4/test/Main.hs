module Main
  ( main
  ) where

import Prelude

import qualified MatrixMultiplyTest as MatrixMultiply
import qualified GeometryTest as Geometry
import qualified GaussTest as Gauss

import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  MatrixMultiply.spec
  Geometry.spec
  Gauss.spec
