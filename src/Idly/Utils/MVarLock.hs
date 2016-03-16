{-# LANGUAGE FlexibleContexts #-}

{-| Utility functions for using MVars as simple locks. -}

module Idly.Utils.MVarLock
  ( Lock()
  , newLock
  , withLock
  ) where

import Control.Exception.Lifted
import Control.Concurrent.MVar.Lifted
import Control.Monad
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))

newtype Lock = MVarLock (MVar ())

newLock :: (MonadBase IO m) => m Lock
newLock = MVarLock `liftM` newMVar ()

withLock :: (MonadBaseControl IO m) => Lock -> m a -> m a
withLock (MVarLock l) = bracket_ (takeMVar l) (putMVar l ())
