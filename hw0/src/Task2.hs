module Task2
    ( Neg
    , doubleNeg
    , excludedNeg
    , pierce
    , doubleNegElim
    , thirdNegElim
    ) where

import Prelude

import Data.Void (Void)

type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
doubleNeg x f = f x

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg f = (f . Right) (f . Left)

-- unprovable in intuitionistic logic
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- unprovable in intuitionistic logic
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim f x = f $ doubleNeg x
