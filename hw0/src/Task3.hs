module Task3
    ( composition
    , identity
    , contraction
    , permutation
    ) where

import Prelude

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- B-combinator
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const

-- I-combinator
identity :: a -> a
identity = s const const

-- W-combinator
contraction :: (a -> a -> b) -> a -> b
contraction = s s (s const)

-- C-combinator
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (const (s (const s) const)) s) (const const)
