module EvalTest
    ( spec
    ) where

import Prelude

import Test.Hspec (SpecWith, describe, it, shouldBe)

import Block2 (ArithmeticError (..), Expr (..), eval)

spec :: SpecWith ()
spec = describe "eval" $ do
  it "returns correct value on simple operations" $
    eval (Sum (Const 2) (Const 3))  `shouldBe` Right 5

  it "returns correct value on more operations" $
    eval ( Sum (Const 2)
           (Mul (Const 3)
           (Sub (Const 5)
           (Div (Const 7) (Const 3)))) )
      `shouldBe` Right 11

  it "returns typed error for division by zero" $
    eval (Div (Const 1) (Const 0)) `shouldBe` Left DivByZero

  it "returns typed error for powering with negative number" $
    eval (Pow (Const 2) (Const (-1))) `shouldBe` Left PowByNegative
