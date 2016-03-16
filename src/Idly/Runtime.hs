{-| Implementation of the runtime configuration details.

-}

module Idly.Runtime
  ( IdlyDaemon(..)
  , RuntimeEnts(..)
  , daemonName
  , daemonOnlyOnMaster
  , daemonLogBase
  , ExtraLogReason(..)
  , daemonLogFile
  , daemonsExtraLogbase
  , daemonsExtraLogFile
  , daemonPidFile
  , verifyDaemonUser
  ) where

import           Control.Monad
import           Control.Monad.Except ()
import qualified Data.Map             as M
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Posix.Types
import           System.Posix.User
import           Text.Printf

import           Idly.BasicTypes      ()
import qualified Idly.ConstantUtils   as ConstantUtils
import qualified Idly.Path            as Path

import           AutoConf             ()

data IdlyDaemon = IdlyMetad
                    deriving (Show, Enum, Bounded, Eq, Ord)


data RuntimeEnts = RuntimeEnts
  { reUserToUid :: M.Map IdlyDaemon UserID
  }

-- | Returns the daemon name for a given daemon.
daemonName :: IdlyDaemon -> String
daemonName IdlyMetad   = "idlyd"


-- | Returns whether the daemon only runs on the master node.
daemonOnlyOnMaster :: IdlyDaemon -> Bool
daemonOnlyOnMaster IdlyMetad   = False


-- | Returns the log file base for a daemon.
daemonLogBase :: IdlyDaemon -> String
daemonLogBase IdlyMetad   = "idlyd"

data ExtraLogReason = AccessLog | ErrorLog

-- | Some daemons might require more than one logfile.  Specifically,
-- right now only the Haskell http library "snap", used by the
-- monitoring daemon, requires multiple log files.
daemonsExtraLogbase :: IdlyDaemon -> ExtraLogReason -> String
daemonsExtraLogbase daemon AccessLog = daemonLogBase daemon ++ "-access"
daemonsExtraLogbase daemon ErrorLog = daemonLogBase daemon ++ "-error"

-- | Returns the log file for a daemon.
daemonLogFile :: IdlyDaemon -> IO FilePath
daemonLogFile daemon = do
  logDir <- Path.logDir
  return $ logDir </> daemonLogBase daemon <.> "log"

-- | Returns the extra log files for a daemon.
daemonsExtraLogFile :: IdlyDaemon -> ExtraLogReason -> IO FilePath
daemonsExtraLogFile daemon logreason = do
  logDir <- Path.logDir
  return $ logDir </> daemonsExtraLogbase daemon logreason <.> "log"

-- | Returns the pid file name for a daemon.
daemonPidFile :: IdlyDaemon -> IO FilePath
daemonPidFile daemon = do
  runDir <- Path.runDir
  return $ runDir </> daemonName daemon <.> "pid"



-- | Checks whether a daemon runs as the right user.
verifyDaemonUser :: IdlyDaemon -> RuntimeEnts -> IO ()
verifyDaemonUser daemon ents = do
  myuid <- getEffectiveUserID
  -- note: we use directly ! as lookup failues shouldn't happen, due
  -- to the above map construction
  checkUidMatch (daemonName daemon) ((M.!) (reUserToUid ents) daemon) myuid

-- | Check that two UIDs are matching or otherwise exit.
checkUidMatch :: String -> UserID -> UserID -> IO ()
checkUidMatch name expected actual =
  when (expected /= actual) $ do
    hPrintf stderr "%s started using wrong user ID (%d), \
                   \expected %d\n" name
              (fromIntegral actual::Int)
              (fromIntegral expected::Int) :: IO ()
    exitWith $ ExitFailure ConstantUtils.exitFailure
