module Block1
    ( stringSum
    , Tree (..)
    ) where

import           Prelude

import           Text.Read (readMaybe)

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

toNonEmpty :: [a] -> NonEmpty a
toNonEmpty (x : xs) = x :| xs
toNonEmpty _        = error "This function calls only on non-empty lists"

instance Functor NonEmpty where
  fmap f (x :| xs) = toNonEmpty $ fmap f (x : xs)

instance Applicative NonEmpty where
  pure f = f :| []

  (f :| fs) <*> (x :| xs) = toNonEmpty $ (f : fs) <*> (x : xs)

instance Foldable NonEmpty where
  foldr f z (x :| xs) = foldr f z (x : xs)

instance Traversable NonEmpty where
  traverse f (x :| xs) = toNonEmpty <$> traverse f (x : xs)
