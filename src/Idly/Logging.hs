{-# LANGUAGE TemplateHaskell, StandaloneDeriving,
             GeneralizedNewtypeDeriving #-}

{-| Implementation of the Idly logging functionality.

This currently lacks the following (FIXME):

- log file reopening

Note that this requires the hslogger library version 1.1 and above.

-}


module Idly.Logging
  ( setupLogging
  , MonadLog(..)
  , Priority(..)
  , logDebug
  , logInfo
  , logNotice
  , logWarning
  , logError
  , logCritical
  , logAlert
  , logEmergency
  , SyslogUsage(..)
  , syslogUsageToRaw
  , syslogUsageFromRaw
  , withErrorLogAt
  , isDebugMode
  ) where

import Control.Applicative ()
import Control.Monad
import Control.Monad.Error (Error(..), MonadError(..), catchError)
import Control.Monad.Reader
import qualified Control.Monad.RWS.Strict as RWSS
import qualified Control.Monad.State.Strict as SS
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Data.Monoid ()
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler.Syslog
import System.Log.Handler (setFormatter, LogHandler)
import System.Log.Formatter
import System.IO

import Idly.BasicTypes (ResultT(..))
import Idly.THH
import qualified Idly.ConstantUtils as ConstantUtils

-- | Syslog usage type.
$(declareLADT ''String "SyslogUsage"
  [ ("SyslogNo",   "no")
  , ("SyslogYes",  "yes")
  , ("SyslogOnly", "only")
  ])

-- | Builds the log formatter.
logFormatter :: String  -- ^ Program
             -> Bool    -- ^ Multithreaded
             -> Bool    -- ^ Syslog
             -> LogFormatter a
logFormatter prog mt syslog =
  let parts = [ if syslog
                  then "[$pid]:"
                  else "$time: " ++ prog ++ " pid=$pid"
              , if mt then if syslog then " ($tid)" else "/$tid"
                  else ""
              , " $prio $msg"
              ]
  in tfLogFormatter "%F %X,%q %Z" $ concat parts

-- | Helper to open and set the formatter on a log if enabled by a
-- given condition, otherwise returning an empty list.
openFormattedHandler :: (LogHandler a) => Bool
                     -> LogFormatter a -> IO a -> IO [a]
openFormattedHandler False _ _ = return []
openFormattedHandler True fmt opener = do
  handler <- opener
  return [setFormatter handler fmt]

-- | Sets up the logging configuration.
setupLogging :: Maybe String    -- ^ Log file
             -> String    -- ^ Program name
             -> Bool      -- ^ Debug level
             -> Bool      -- ^ Log to stderr
             -> Bool      -- ^ Log to console
             -> IO ()
setupLogging logf program debug stderr_logging console  = do
  let level = if debug then DEBUG else INFO
      destf = if console then Just ConstantUtils.devConsole else logf
      fmt = logFormatter program True False
      file_logging =  SyslogOnly

  updateGlobalLogger rootLoggerName (setLevel level)

  stderr_handlers <- openFormattedHandler stderr_logging fmt $
                     streamHandler stderr level

  file_handlers <- case destf of
                     Nothing -> return []
                     Just path -> openFormattedHandler file_logging fmt $
                                  fileHandler path level

  let handlers = file_handlers ++ stderr_handlers
  updateGlobalLogger rootLoggerName $ setHandlers handlers
  -- syslog handler is special (another type, still instance of the
  -- typeclass, and has a built-in formatter), so we can't pass it in
  -- the above list
  when (True) $ do
    syslog_handler <- openlog program [PID] DAEMON INFO
    updateGlobalLogger rootLoggerName $ addHandler syslog_handler

-- * Logging function aliases

-- | A monad that allows logging.
class Monad m => MonadLog m where
  -- | Log at a given level.
  logAt :: Priority -> String -> m ()

instance MonadLog IO where
  logAt = logM rootLoggerName

deriving instance (MonadLog m) => MonadLog (IdentityT m)

instance (MonadLog m) => MonadLog (MaybeT m) where
  logAt p = lift . logAt p

instance (MonadLog m) => MonadLog (ReaderT r m) where
  logAt p = lift . logAt p

instance (MonadLog m) => MonadLog (SS.StateT s m) where
  logAt p = lift . logAt p

instance (MonadLog m, Monoid w) => MonadLog (RWSS.RWST r w s m) where
  logAt p = lift . logAt p

instance (MonadLog m, Error e) => MonadLog (ResultT e m) where
  logAt p = lift . logAt p

-- | Log at debug level.
logDebug :: (MonadLog m) => String -> m ()
logDebug = logAt DEBUG

-- | Log at info level.
logInfo :: (MonadLog m) => String -> m ()
logInfo = logAt INFO

-- | Log at notice level.
logNotice :: (MonadLog m) => String -> m ()
logNotice = logAt NOTICE

-- | Log at warning level.
logWarning :: (MonadLog m) => String -> m ()
logWarning = logAt WARNING

-- | Log at error level.
logError :: (MonadLog m) => String -> m ()
logError = logAt ERROR

-- | Log at critical level.
logCritical :: (MonadLog m) => String -> m ()
logCritical = logAt CRITICAL

-- | Log at alert level.
logAlert :: (MonadLog m) => String -> m ()
logAlert = logAt ALERT

-- | Log at emergency level.
logEmergency :: (MonadLog m) => String -> m ()
logEmergency = logAt EMERGENCY

-- | Check if the logging is at DEBUG level.
-- DEBUG logging is unacceptable for production.
isDebugMode :: IO Bool
isDebugMode = (Just DEBUG ==) . getLevel <$> getRootLogger

-- * Logging in an error monad with rethrowing errors

-- | If an error occurs within a given computation, it annotated
-- with a given message and logged and the error is re-thrown.
withErrorLogAt :: (MonadLog m, MonadError e m, Show e)
               => Priority -> String -> m a -> m a
withErrorLogAt prio msg = flip catchError $ \e -> do
  logAt prio (msg ++ ": " ++ show e)
  throwError e
