{-# OPTIONS -fno-warn-type-defaults #-}
{-| Constants contains the Haskell constants

The constants in this module are used in Haskell and are also
converted to Python.

Do not write any definitions in this file other than constants.  Do
not even write helper functions.  The definitions in this module are
automatically stripped to build the Makefile.am target
'ListConstants.hs'.  If there are helper functions in this module,
they will also be dragged and it will cause compilation to fail.
Therefore, all helper functions should go to a separate module and
imported.

-}

module Idly.Constants where

import           Control.Arrow      ((&&&), (***))
import           Data.List          ((\\))
import           Data.Map           (Map)
import qualified Data.Map           as Map (empty, fromList, insert, keys)

import qualified AutoConf
import           Idly.ConstantUtils (FrozenSet, Protocol (..), buildVersion)
import qualified Idly.ConstantUtils as ConstantUtils
import           Idly.Logging       (SyslogUsage (..))
import qualified Idly.Logging       as Logging (syslogUsageToRaw)
import           Idly.Runtime       (ExtraLogReason (..), IdlyDaemon (..))
import qualified Idly.Runtime       as Runtime
import           Idly.Types
import qualified Idly.Types         as Types

{-# ANN module "HLint: ignore Use camelCase" #-}


-- ** Build-time constants

localstatedir :: String
localstatedir = AutoConf.localstatedir


-- * Various versions

versionMajor :: Int
versionMajor = AutoConf.versionMajor

versionMinor :: Int
versionMinor = AutoConf.versionMinor


-- * Daemons

metad :: String
metad = Runtime.daemonName IdlyMetad


defaultMetadPort :: Int
defaultMetadPort = 1818


daemonsPorts :: Map String (Protocol, Int)
daemonsPorts =
  Map.fromList
  [ (metad, (Tcp, defaultMetadPort)) ]


devConsole :: String
devConsole = ConstantUtils.devConsole


-- * Syslog

syslogNo :: String
syslogNo = Logging.syslogUsageToRaw SyslogNo

syslogYes :: String
syslogYes = Logging.syslogUsageToRaw SyslogYes

syslogOnly :: String
syslogOnly = Logging.syslogUsageToRaw SyslogOnly


-- | Path generating random UUID
randomUuidFile :: String
randomUuidFile = ConstantUtils.randomUuidFile

-- * UUID regex

uuidRegex :: String
uuidRegex = "^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$"


-- * Error related constants
--
-- 'OpPrereqError' failure types

-- | Environment error (e.g. node disk error)
errorsEcodeEnviron :: String
errorsEcodeEnviron = "environment_error"

-- | Entity already exists
errorsEcodeExists :: String
errorsEcodeExists = "already_exists"

-- | Internal cluster error
errorsEcodeFault :: String
errorsEcodeFault = "internal_error"

-- | Wrong arguments (at syntax level)
errorsEcodeInval :: String
errorsEcodeInval = "wrong_input"

-- | Entity not found
errorsEcodeNoent :: String
errorsEcodeNoent = "unknown_entity"

-- | Not enough resources (iallocator failure, disk space, memory, etc)
errorsEcodeNores :: String
errorsEcodeNores = "insufficient_resources"

-- | Resource not unique (e.g. MAC or IP duplication)
errorsEcodeNotunique :: String
errorsEcodeNotunique = "resource_not_unique"

-- | Resolver errors
errorsEcodeResolver :: String
errorsEcodeResolver = "resolver_error"

-- | Wrong entity state
errorsEcodeState :: String
errorsEcodeState = "wrong_state"

-- | Temporarily out of resources; operation can be tried again
errorsEcodeTempNores :: String
errorsEcodeTempNores = "temp_insufficient_resources"

errorsEcodeAll :: FrozenSet String
errorsEcodeAll =
  ConstantUtils.mkSet [ errorsEcodeNores
                      , errorsEcodeExists
                      , errorsEcodeState
                      , errorsEcodeNotunique
                      , errorsEcodeTempNores
                      , errorsEcodeNoent
                      , errorsEcodeFault
                      , errorsEcodeResolver
                      , errorsEcodeInval
                      , errorsEcodeEnviron
                      ]

                    -- * Common exit codes

exitSuccess :: Int
exitSuccess = 0

exitFailure :: Int
exitFailure = ConstantUtils.exitFailure

exitNotcluster :: Int
exitNotcluster = 5

exitNotmaster :: Int
exitNotmaster = 11

exitNodesetupError :: Int
exitNodesetupError = 12

-- | Need user confirmation
exitConfirmation :: Int
exitConfirmation = 13

-- | Exit code for query operations with unknown fields
exitUnknownField :: Int
exitUnknownField = 14

cvAllEcodesStrings :: FrozenSet String
cvAllEcodesStrings =
  ConstantUtils.mkSet $ map Types.cVErrorCodeToRaw [minBound..]


syslogUsage :: String
syslogUsage = syslogYes

-- | SSCONF file prefix
ssconfFileprefix :: String
ssconfFileprefix = "ssconf_"

-- | Each request is "salted" by the current timestamp.
--
-- This constant decides how many seconds of skew to accept.
--
-- TODO: make this a default and allow the value to be more
-- configurable
-- | Node clock skew (seconds)
nodeMaxClockSkew :: Int
nodeMaxClockSkew = 150


confdMaxClockSkew :: Int
confdMaxClockSkew = 2 * nodeMaxClockSkew

-- | When we haven't reloaded the config for more than this amount of
-- seconds, we force a test to see if inotify is betraying us. Using a
-- prime number to ensure we get less chance of 'same wakeup' with
-- other processes.
confdConfigReloadTimeout :: Int
confdConfigReloadTimeout = 17

-- | If we receive more than one update in this amount of
-- microseconds, we move to polling every RATELIMIT seconds, rather
-- than relying on inotify, to be able to serve more requests.
confdConfigReloadRatelimit :: Int
confdConfigReloadRatelimit = 250000
