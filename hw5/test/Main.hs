module Main
    ( main
    ) where

import Prelude

import Test.Hspec (hspec)

import qualified PairLensTest as PairLens
import qualified STTest as ST

main :: IO ()
main = hspec $ do
  PairLens.spec
  ST.spec
