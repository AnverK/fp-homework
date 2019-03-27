module Bonus
    ( Cont (..)
    ) where

import Prelude

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
  fmap f (Cont arr) = Cont $ \br -> arr (br . f)

instance Applicative (Cont r) where
  pure a = Cont $ \ar -> ar a

  (Cont f) <*> (Cont arr) = Cont $ \br ->
    f $ \g ->
    arr $ \a ->
    br (g a)

instance Monad (Cont r) where
  return = pure

  (Cont arr) >>= f = Cont $ \br ->
    arr $ \a ->
    runCont (f a) br
