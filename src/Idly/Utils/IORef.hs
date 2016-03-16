{-# LANGUAGE FlexibleContexts, RankNTypes #-}

{-| Utility functions for working with IORefs. -}

module Idly.Utils.IORef
  ( atomicModifyWithLens
  , atomicModifyIORefErr
  , atomicModifyIORefErrLog
  ) where

import Control.Monad
import Control.Monad.Base
import Data.IORef.Lifted
import Data.Tuple (swap)

import Idly.BasicTypes
import Idly.Lens
import Idly.Logging
import Idly.Logging.WriterLog

-- | Atomically modifies an 'IORef' using a lens
atomicModifyWithLens :: (MonadBase IO m)
                     => IORef a -> Lens a a b c -> (b -> (r, c)) -> m r
atomicModifyWithLens ref l f = atomicModifyIORef ref (swap . traverseOf l f)

-- | Atomically modifies an 'IORef' using a function that can possibly fail.
-- If it fails, the value of the 'IORef' is preserved.
atomicModifyIORefErr :: (MonadBase IO m)
                     => IORef a -> (a -> GenericResult e (a, b))
                     -> ResultT e m b
atomicModifyIORefErr ref f =
  let f' x = genericResult ((,) x . Bad) (fmap Ok) (f x)
   in ResultT $ atomicModifyIORef ref f'

-- | Atomically modifies an 'IORef' using a function that can possibly fail
-- and log errors.
-- If it fails, the value of the 'IORef' is preserved.
-- Any log messages are passed to the outer monad.
atomicModifyIORefErrLog :: (MonadBase IO m, MonadLog m)
                        => IORef a -> (a -> ResultT e WriterLog (a, b))
                        -> ResultT e m b
atomicModifyIORefErrLog ref f = ResultT $ do
  let f' x = let ((a, b), w) = runWriterLog
                              . liftM (genericResult ((,) x . Bad) (fmap Ok))
                              . runResultT $ f x
             in (a, (b, w))
  (b, w) <- atomicModifyIORef ref f'
  dumpLogSeq w
  return b
