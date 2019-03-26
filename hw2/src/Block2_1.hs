{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Block2_1
    ( MonadFish
    , MonadJoin
    , Monad
    , return
    , (>>=)
    , returnJoin
    , join
    ) where

import Prelude hiding (Monad, return, (>>=))

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    return :: a -> m a

instance MonadFish m => Monad m where
  return = returnFish
  ma >>= mf = (id >=> mf) ma

instance MonadFish m => MonadJoin m where
  returnJoin = returnFish
  join = id >=> id
