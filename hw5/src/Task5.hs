{-# LANGUAGE Rank2Types #-}

module Task5
    ( Lens'
    , set
    , view
    , over
    , _1
    , _2
    , lens
    , choosing
    , (<%~)
    , (<<%~)
    ) where

import Data.Functor.Const (Const (..), getConst)
import Data.Functor.Identity (Identity (..), runIdentity)

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

type Lens' s a  = Lens s s a a

-- set  :: Lens' s a -> a -> s -> s         -- set    value (setter)
-- set lns val = over lns (const val)
--
-- view :: Lens' s a -> s -> a              -- lookup value (getter)
-- view lns obj = getConst $ lns Const obj
--
-- over :: Lens' s a -> (a -> a) -> s -> s  -- change value (modifier)
-- over lns f obj = runIdentity $ lns (Identity . f) obj

view :: Lens s t a b -> s -> a              -- lookup value (getter)
view lns obj = getConst $ lns Const obj

over :: Lens s t a b -> (a -> b) -> s -> t  -- change value (modifier)
over lns f obj = runIdentity $ lns (Identity . f) obj

set :: Lens s t a b -> b -> s -> t          -- set value (setter)
set lns val = over lns (const val)

_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = flip (,) x <$> f a

_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (,) x <$> f a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get' set' f obj = set' obj <$> f (get' obj)

choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = lens (either (view l1) (view l2)) $ setter l1 l2
  where
    setter :: Lens s1 t1 a b
           -> Lens s2 t2 a b
           -> Either s1 s2 -> b -> Either t1 t2
    setter l1' _ (Left obj) val  = Left $ set l1' val obj
    setter _ l2' (Right obj) val = Right $ set l2' val obj

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = (f $ view l s, over l f s)

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = (view l s, over l f s)
