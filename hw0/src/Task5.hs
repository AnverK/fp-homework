module Task5
    ( Nat
    , zero
    , succChurch
    , churchPlus
    , churchMult
    , churchToInt
    ) where

import Prelude

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero _ x = x

succChurch :: Nat a -> Nat a
succChurch nat f x = nat f (f x)

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus n m f x = n f (m f x)

churchMult :: Nat a -> Nat a -> Nat a
churchMult m n f = m (n f)

churchToInt :: Nat Integer -> Integer
churchToInt n = n (+1) 0
