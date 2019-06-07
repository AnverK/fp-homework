{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}

module Task1
    ( ST
    , STRef
    , newSTRef
    , writeSTRef
    , readSTRef
    , runST
    ) where

import Prelude

import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Data.Typeable (Typeable, cast)

newtype Tag = Tag Int

newtype MOjbect s a = MObject (Tag -> (a, Tag))
newtype Object s a = Object Tag
newtype STRef s a = STRef (Object s a)

instance Functor (MOjbect s) where
  fmap f (MObject g) = MObject (\n -> (f $ fst $ g n, n))

instance Applicative (MOjbect s) where
  pure v = MObject (\n -> (v, n))

  MObject f <*> MObject a = MObject (\n -> (fst (f n) $ fst $ a n, n))

instance Monad (MOjbect s) where
  return x    = MObject (\n -> (x, n))

  MObject m >>= f = MObject (\n -> let (a, n') = m n
                                       MObject m' = f a
                                   in m' n')

createObject :: Typeable a => MOjbect s (Object s a)
createObject = MObject (\(Tag n) -> (Object $ Tag n, Tag $ n + 1))

maybeCast :: (Typeable a, Typeable b) => Object s a -> Object s b -> Maybe (a -> Maybe b)
maybeCast (Object (Tag n)) (Object (Tag k))
  | n == k    = Just cast
  | otherwise = Nothing


data    Entry s   = forall a . Typeable a => Entry (Object s a) a
type    State s   = [Entry s]
newtype ST    s a = ST (State s -> MOjbect s (a, State s))

instance Functor (ST s)
  where
    fmap f (ST a) = ST (\st -> flip (,) st . f . fst <$> a st)

instance Applicative (ST s)
  where
    pure a = ST (\st -> pure (a, st))

    (ST f) <*> (ST a) = ST (\st -> let a' = fst <$> a st
                                       f' = fst <$> f st
                                   in flip (,) st <$> (f' <*> a'))

instance Monad (ST s) where
  return = pure

  ST m >>= f = ST (\st -> do
    (a, st') <- m st
    let ST m' = f a
    m' st')

newSTRef :: Typeable a => a -> ST s (STRef s a)
newSTRef val = ST $ \st ->
  do obj <- createObject
     return (STRef obj, Entry obj val : st)

readSTRef :: Typeable a => STRef s a -> ST s a
readSTRef (STRef tp) = ST $ \st -> return (readFromState st tp, st)
  where
    readFromState :: Typeable a => State s -> Object s a -> a
    readFromState state obj2 =
      let Just entry = find (\(Entry obj1 _) -> isJust $ maybeCast obj1 obj2) state
      in case entry of
        Entry obj val -> fromJust $ fromJust (maybeCast obj obj2) val

writeSTRef :: Typeable a => STRef s a -> a -> ST s ()
writeSTRef (STRef tp) a = ST $ \st -> return ((), update st tp a)
 where
    update :: Typeable a => State s -> Object s a -> a -> State s
    update state obj2 newVal =
      let Just entry = find (\(Entry obj1 _) -> isJust $ maybeCast obj1 obj2) state
      in case entry of
        Entry obj _ -> let res = fromJust $ fromJust (maybeCast obj2 obj) newVal
                         in Entry obj res : state

runMOjbect :: MOjbect s a -> a
runMOjbect (MObject f) = fst $ f $ Tag 0

runST :: (forall s . ST s a) -> a
runST (ST f) = runMOjbect $ do
  (a, _) <- f []
  return a
