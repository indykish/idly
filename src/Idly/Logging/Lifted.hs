{-| Idly logging functions expressed using MonadBase

This allows to use logging functions without having instances for all
possible transformers.

-}

module Idly.Logging.Lifted
  ( MonadLog()
  , Priority(..)
  , L.withErrorLogAt
  , L.isDebugMode
  , logAt
  , logDebug
  , logInfo
  , logNotice
  , logWarning
  , logError
  , logCritical
  , logAlert
  , logEmergency
  ) where

import Control.Monad.Base

import Idly.Logging (MonadLog, Priority(..))
import qualified Idly.Logging as L

-- * Logging function aliases for MonadBase

-- | A monad that allows logging.
logAt :: (MonadLog b, MonadBase b m) => Priority -> String -> m ()
logAt p = liftBase . L.logAt p

-- | Log at debug level.
logDebug :: (MonadLog b, MonadBase b m) => String -> m ()
logDebug = logAt DEBUG

-- | Log at info level.
logInfo :: (MonadLog b, MonadBase b m) => String -> m ()
logInfo = logAt INFO

-- | Log at notice level.
logNotice :: (MonadLog b, MonadBase b m) => String -> m ()
logNotice = logAt NOTICE

-- | Log at warning level.
logWarning :: (MonadLog b, MonadBase b m) => String -> m ()
logWarning = logAt WARNING

-- | Log at error level.
logError :: (MonadLog b, MonadBase b m) => String -> m ()
logError = logAt ERROR

-- | Log at critical level.
logCritical :: (MonadLog b, MonadBase b m) => String -> m ()
logCritical = logAt CRITICAL

-- | Log at alert level.
logAlert :: (MonadLog b, MonadBase b m) => String -> m ()
logAlert = logAt ALERT

-- | Log at emergency level.
logEmergency :: (MonadLog b, MonadBase b m) => String -> m ()
logEmergency = logAt EMERGENCY
