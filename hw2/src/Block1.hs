module Block1
    ( stringSum
    , Tree (..)
    ) where

import Prelude

import Text.Read (readMaybe)
import Control.Applicative (liftA2)

stringSum :: String -> Maybe Int
stringSum s = sum <$> mapM readMaybe (words s)

--------------------------------------------------------------------------------

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a

instance Functor Tree where
  fmap f (Leaf a)     = Leaf $ f a
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
  pure = Leaf

  (Leaf f) <*> (Leaf a)         = Leaf $ f a
  (Leaf f) <*> (Branch l r)     = Branch (f <$> l) (f <$> r)
  (Branch f g) <*> (Branch l r) = Branch (f <*> l) (g <*> r)
  (Branch f g) <*> leaf         = Branch (f <*> leaf) (g <*> leaf)

instance Foldable Tree where
  foldr f z (Leaf a)     = f a z
  foldr f z (Branch l r) = foldr f (foldr f z l) r

instance Traversable Tree where
  traverse f (Leaf a)     = Leaf <$> f a
  traverse f (Branch l r) = Branch <$> traverse f l <*> traverse f r

--------------------------------------------------------------------------------

data NonEmpty a = a :| [a] deriving Show

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure f = f :| []

  (f :| fs) <*> (x :| xs) = f x :| ((f <$> xs) ++ (fs <*> (x : xs)))

instance Foldable NonEmpty where
  foldr f z (x :| xs) = f x $ foldr f z xs

instance Traversable NonEmpty where
  traverse f (x :| xs) = liftA2 (:|) (f x) (traverse f xs)
