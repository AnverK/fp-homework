{-# LANGUAGE LambdaCase #-}

module Task5
    ( AllocateT (..)
    , allocate
    , release
    , runAllocateT
    ) where

import Prelude hiding (lookup)

import Control.Exception (Exception, SomeException, mask_, throw, throwIO, try)
import qualified Control.Monad.Catch as Catch (MonadMask, bracket, catchAll)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.IntMap (IntMap, delete, elems, empty, insert, lookup)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes, fromMaybe)

newtype AllocateT m a = AllocateT { unAllocateT :: IORef Pointers -> m a }

instance MonadTrans AllocateT where
    lift m = AllocateT (const m)

-- for more convenient testing
instance Functor m => Functor (AllocateT m) where
    fmap f (AllocateT m) = AllocateT $ \r -> fmap f (m r)

instance Applicative m => Applicative (AllocateT m) where
    pure = AllocateT . const . pure
    AllocateT mf <*> AllocateT ma = AllocateT $ \r ->
        mf r <*> ma r

instance Monad m => Monad (AllocateT m) where
    return = pure
    AllocateT ma >>= f = AllocateT $ \r -> do
        a <- ma r
        let AllocateT f' = f a
        f' r

data Pointers
  = Nullptr
  | Pointer Int (IntMap (ReleaseType -> IO ()))

data ReleaseType
  = ReleaseNormal
  | ReleaseError

newtype InvalidAccess = InvalidAccess String
  deriving (Show)

instance Exception InvalidAccess

data ResourceKey = ResourceKey (IORef Pointers) Int

data ReleaseException = ReleaseException
  deriving (Show)

instance Exception ReleaseException

allocate :: MonadIO m => IO a -> (a -> IO ()) -> AllocateT m (a, ResourceKey)
allocate acq rel = AllocateT (liftIO . f)
  where
    f = \curMap -> liftIO $ mask_ $ do
        res <- acq
        key <- getResourseKey curMap $ rel res
        return (res, key)
        where
          getResourseKey :: IORef Pointers -> IO () -> IO ResourceKey
          getResourseKey curMap relAction = atomicModifyIORef curMap $ \case
            Nullptr             ->
              throw $ InvalidAccess "Map to release actions is invalid"
            Pointer key actions ->
              ( Pointer (key + 1) $ insert key (const relAction) actions
              , ResourceKey curMap key
              )

release :: MonadIO m => ResourceKey -> AllocateT m ()
release (ResourceKey pointers key) =
  let f = liftIO $ release' (fromMaybe (pure ()))
  in AllocateT (const f)

  where
    release' :: (Maybe (IO ()) -> IO a) -> IO a
    release' act = mask_ $ do
        maction <- atomicModifyIORef pointers getAction
        act maction
      where
        getAction :: Pointers -> (Pointers, Maybe (IO ()))
        getAction Nullptr                      = (Nullptr, Nothing)
        getAction rm@(Pointer next relActions) = case lookup key relActions of
          Nothing -> (rm, Nothing)
          Just action ->
            ( Pointer next $ delete key relActions
            , Just (action ReleaseNormal)
            )

readMap :: Pointers -> Either InvalidAccess (IntMap (ReleaseType -> IO ()))
readMap Nullptr       = Left $ InvalidAccess "access to wrong key"
readMap (Pointer _ x) = Right x

-- This function tries to release resources if possible (if destructors throwing
-- new exceptions, it will also throw exception) and returns.
-- if `releaseAllResources` was called without any Exception, then it will
-- return exception if and only if any of destructors will throw them. Otherwise,
-- this function will return Exception
releaseAllResources :: Maybe SomeException -> IORef Pointers -> IO ()
releaseAllResources maybeException pointers = mask_ $ do
    ptr <- readIORef pointers
    let eitherMap = readMap ptr
    case eitherMap of
      Left e -> throw e
      Right m -> do
        errors <- applyReleases (\x -> tryMaybe (x $ releaseType maybeException)) $ elems m
        _ <- writeIORef pointers $ Pointer minBound empty
        case errors of
            [] -> return ()
            _  -> throwIO ReleaseException
  where
    tryMaybe :: IO () -> IO (Maybe SomeException)
    tryMaybe relAction = either Just (\() -> Nothing) <$> try relAction

    releaseType :: Maybe SomeException -> ReleaseType
    releaseType Nothing = ReleaseNormal
    releaseType _       = ReleaseError

    applyReleases :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
    applyReleases f rels = do
      res <- mapM f $ reverse rels
      return $ catMaybes res

emptyAllocate :: MonadIO m => m (IORef Pointers)
emptyAllocate = liftIO $ newIORef $ Pointer minBound empty

runAllocateT :: (Catch.MonadMask m, MonadIO m) => AllocateT m a -> m a
runAllocateT (AllocateT r) =
  Catch.bracket
    emptyAllocate
    (liftIO . releaseAllResources Nothing)
    (\pointers -> r pointers `Catch.catchAll` \e -> do
        liftIO $ releaseAllResources (Just e) pointers
        liftIO $ throwIO e)
