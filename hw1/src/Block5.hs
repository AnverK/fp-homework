{-# LANGUAGE InstanceSigs #-}

module Block5
    ( maybeConcat
    , eitherConcat
    , NonEmpty (..)
    , ThisOrThat (..)
    , Builder (..)
    , Name (..)
    , Endo (..)
    , fromString
    , toString
    ) where

import Prelude

import Data.Either (rights)
import Data.List (map)
import Data.Monoid (mconcat)

maybeConcat :: [Maybe[a]] -> [a]
maybeConcat []               = []
maybeConcat (Nothing : rest) = maybeConcat rest
maybeConcat (Just x:rest)    = x ++ maybeConcat rest

eitherConcat :: (Monoid ml, Monoid mr) => [Either ml mr] -> (ml, mr)
eitherConcat x = (mconcat $ myLefts x, mconcat $ rights x)
  where
    myLefts :: [Either a b] -> [a]
    myLefts l = [ a | (Left a) <- l ]
    -- rights is similar, I just don't know which way was expected

--------------------------------------------------------------------------------

data NonEmpty a = a :| [a] deriving (Show)
--
instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (<>) (x :| restX) (y :| restY) = x :| (restX ++ (y : restY))

data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving (Show)

-- 1. x is Both.
--      (x <> y) <> z = x <> z = x
--      x <> (y <> z) =          x  (doesn't matter whatever (y <> z) was)
-- 2. x isn't Both.
--      (x <> y) <> z = y <> z
--      x <> (y <> z) = y <> z
instance Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  (<>) bo@(Both _ _) _ = bo
  (<>) _ b             = b

newtype Name = Name String deriving (Show)

-- It is easy to look for 8 (even less) cases to understand that this
-- function is associative.
instance Semigroup Name where
  (<>) :: Name -> Name -> Name
  (<>) a (Name "")       = a
  (<>) (Name "") a       = a
  (<>) (Name a) (Name b) = Name (a ++ ('.' : b))

-- Obviously it is neutral element for (<>)
instance Monoid Name where
  mempty :: Name
  mempty = Name ""

newtype Endo a = Endo
  { getEndo :: a -> a
  }

-- is Semigroup like composition of functions
instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (<>) (Endo a) (Endo b) = Endo (a . b)

instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo id
--------------------------------------------------------------------------------

data Builder
  = One Char
  | Many [Builder]
  deriving (Show)

-- Obviously is associative
instance Semigroup Builder where
  (<>) :: Builder -> Builder -> Builder
  (<>) a@(One _) b@(One _) = Many [a, b]
  (<>) a@(One _) (Many l)  = Many (a : l)
  (<>) l@(Many _) r        = Many [l, r]

instance Monoid Builder where
  mempty :: Builder
  mempty = Many []

fromString :: String -> Builder
fromString s = mconcat $ map One s

toString :: Builder -> String
toString (One c)  = [c]
toString (Many b) = mconcat $ map toString b
