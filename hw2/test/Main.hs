module Main
  ( main
  ) where

import Prelude

import qualified EvalTest as Eval
import qualified ParserTest as Parser
import qualified StringSumTest as StringSum

import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  StringSum.spec
  Eval.spec
  Parser.spec
