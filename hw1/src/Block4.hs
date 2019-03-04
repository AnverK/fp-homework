{-# LANGUAGE InstanceSigs #-}

module Block4
       ( Pair (..)
       , NonEmpty (..)
       , splitOn
       , joinWith
       ) where

data Pair a = Pair a a
data NonEmpty a = a :| [a] deriving (Show)

instance Foldable Pair where
  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr f z (Pair l r) = l `f` (r `f` z)

  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair l r) = f l <> f r

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (a :| [])       = a `f` z
  foldr f z (a :| (x : xs)) = a `f` foldr f z (x :| xs)

  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (a :| [])       = f a
  foldMap f (a :| (x : xs)) = f a <> foldMap f (x :| xs)

--------------------------------------------------------------------------------

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn el = foldr (splitHelper el) ([] :| [])
  where
    splitHelper :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
    splitHelper search curElem (curList :| rest)
      | search == curElem = [] :| (curList : rest)
      | otherwise         = (curElem : curList) :| rest

joinWith :: Eq a => a -> NonEmpty [a] -> [a]
joinWith el (x :| xs) = foldl (\prev cur -> prev ++ (el : cur)) x xs
