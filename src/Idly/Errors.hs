{-# LANGUAGE TemplateHaskell #-}

{-| Implementation of the Idly error types.

This module implements our error hierarchy. Currently we implement one
identical to the Python one; later we might one to have separate ones
for frontend (clients), master and backend code.

-}

module Idly.Errors
  ( ErrorCode(..)
  , IdlyException(..)
  , ErrorResult
  , errToResult
  , errorExitCode
  , excName
  , formatError
  , ResultG
  , maybeToError
  ) where

import           System.Exit
import           Text.JSON       hiding (Ok, Result)

import           Idly.BasicTypes
import qualified Idly.Constants  as C
import           Idly.THH

-- | Error code types for 'OpPrereqError'.
$(declareSADT "ErrorCode"
  [ ("ECodeResolver",  'C.errorsEcodeResolver)
  , ("ECodeNoRes",     'C.errorsEcodeNores)
  , ("ECodeTempNoRes", 'C.errorsEcodeTempNores)
  , ("ECodeInval",     'C.errorsEcodeInval)
  , ("ECodeState",     'C.errorsEcodeState)
  , ("ECodeNoEnt",     'C.errorsEcodeNoent)
  , ("ECodeExists",    'C.errorsEcodeExists)
  , ("ECodeNotUnique", 'C.errorsEcodeNotunique)
  , ("ECodeFault",     'C.errorsEcodeFault)
  , ("ECodeEnviron",   'C.errorsEcodeEnviron)
  ])
$(makeJSONInstance ''ErrorCode)

$(genException "IdlyException"
  [ ("GenericError", [excErrMsg])
  , ("LockError", [excErrMsg])
  , ("PidFileLockError", [excErrMsg])
  , ("HypervisorError", [excErrMsg])
  , ("ProgrammerError", [excErrMsg])
  , ("BlockDeviceError", [excErrMsg])
  , ("ConfigurationError", [excErrMsg])
  , ("ConfigVerifyError", [excErrMsg, ("allErrors", [t| [String] |])])
  , ("ConfigVersionMismatch", [ ("expVer", [t| Int |])
                              , ("actVer", [t| Int |])])
  , ("ReservationError", [excErrMsg])
  , ("RemoteError", [excErrMsg])
  , ("SignatureError", [excErrMsg])
  , ("ParameterError", [excErrMsg])
  , ("ResultValidationError", [excErrMsg])
  , ("OpPrereqError", [excErrMsg, ("errCode", [t| ErrorCode |])])
  , ("OpExecError", [excErrMsg])
  , ("OpResultError", [excErrMsg])
  , ("OpCodeUnknown", [excErrMsg])
  , ("JobLost", [excErrMsg])
  , ("JobFileCorrupted", [excErrMsg])
  , ("ResolverError", [ ("errHostname", [t| String |])
                      , ("errResolverCode", [t| Int |])
                      , ("errResolverMsg", [t| String |])])
  , ("HooksFailure", [excErrMsg])
  , ("HooksAbort", [("errs", [t| [(String, String, String)] |])])
  , ("UnitParseError", [excErrMsg])
  , ("ParseError", [excErrMsg])
  , ("TypeEnforcementError", [excErrMsg])
  , ("X509CertError", [ ("certFileName", [t| String |])
                      , excErrMsg ])
  , ("TagError", [excErrMsg])
  , ("CommandError", [excErrMsg])
  , ("StorageError", [excErrMsg])
  , ("InotifyError", [excErrMsg])
  , ("JobQueueError", [excErrMsg])
  , ("JobQueueDrainError", [excErrMsg])
  , ("JobQueueFull", [])
  , ("ConfdMagicError", [excErrMsg])
  , ("ConfdClientError", [excErrMsg])
  , ("UdpDataSizeError", [excErrMsg])
  , ("NoCtypesError", [excErrMsg])
  , ("IPAddressError", [excErrMsg])
  , ("LuxiError", [excErrMsg])
  , ("QueryFilterParseError", [excErrMsg]) -- not consistent with Python
  , ("RapiTestResult", [excErrMsg])
  , ("FileStoragePathError", [excErrMsg])
  ])

instance Error IdlyException where
  strMsg = GenericError

instance JSON IdlyException where
  showJSON = saveIdlyException
  readJSON = loadIdlyException

-- | Error monad using 'IdlyException' type alias.
type ErrorResult = GenericResult IdlyException

$(genStrOfOp ''IdlyException "excName")

-- | Returns the exit code of a program that should be used if we got
-- back an exception from masterd.
errorExitCode :: IdlyException -> ExitCode
errorExitCode (ConfigurationError {}) = ExitFailure 2
errorExitCode (ConfigVerifyError {}) = ExitFailure 2
errorExitCode _ = ExitFailure 1

-- | Formats an exception.
formatError :: IdlyException -> String
formatError (ConfigurationError msg) =
  "Corrupt configuration file: " ++ msg ++ "\nAborting."
formatError (ConfigVerifyError msg es) =
  "Corrupt configuration file: " ++ msg ++ "\nAborting. Details:\n"
  ++ unlines es
formatError (HooksAbort errs) =
  unlines $
  "Failure: hooks execution failed:":
  map (\(node, script, out) ->
         "  node: " ++ node ++ ", script: " ++ script ++
                    if null out
                      then " (no output)"
                      else ", output: " ++ out
      ) errs
formatError (HooksFailure msg) =
  "Failure: hooks general failure: " ++ msg
formatError (ResolverError host _ _) =
  -- FIXME: in Python, this uses the system hostname to format the
  -- error differently if we are failing to resolve our own hostname
  "Failure: can't resolve hostname " ++ host
formatError (OpPrereqError msg code) =
  "Failure: prerequisites not met for this" ++
  " operation:\nerror type: " ++ show code ++ ", error details:\n" ++ msg
formatError (OpExecError msg) =
  "Failure: command execution error:\n" ++ msg
formatError (TagError msg) =
  "Failure: invalid tag(s) given:\n" ++ msg
formatError (JobQueueDrainError _)=
  "Failure: the job queue is marked for drain and doesn't accept new requests"
formatError JobQueueFull =
  "Failure: the job queue is full and doesn't accept new" ++
  " job submissions until old jobs are archived"
formatError (TypeEnforcementError msg) =
  "Parameter Error: " ++ msg
formatError (ParameterError msg) =
  "Failure: unknown/wrong parameter name '" ++ msg ++ "'"
formatError (JobLost msg) =
  "Error checking job status: " ++ msg
formatError (QueryFilterParseError msg) =
  -- FIXME: in Python, this has a more complex error message
  "Error while parsing query filter: " ++ msg
formatError (GenericError msg) =
  "Unhandled Idly error: " ++ msg
formatError err =
  "Unhandled exception: " ++ show err

-- | A type for IO actions with errors properly handled as
-- 'IdlyException's.
-- TODO: Move to Errors.hs
type ResultG = ResultT IdlyException IO

-- | Convert from an 'ErrorResult' to a standard 'Result'.
errToResult :: ErrorResult a -> Result a
errToResult (Ok a)  = Ok a
errToResult (Bad e) = Bad $ formatError e

-- | Convert from a 'Maybe' to a an 'ErrorResult'.
maybeToError :: String -> Maybe a -> ErrorResult a
maybeToError _ (Just a) = Ok a
maybeToError m  Nothing = Bad $ GenericError m
