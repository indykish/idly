{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| Unittests for idly haskell common lib.

-}

module Test.Idly.Daemon (testDaemon) where

import           Test.HUnit
import           Test.QuickCheck      hiding (Result)

import           Test.Idly.Common
import           Test.Idly.TestCommon
import           Test.Idly.TestHelper

import           Idly.Common
import           Idly.Daemon          as Daemon

{-# ANN module "HLint: ignore Use camelCase" #-}

-- | Test a few string arguments.
prop_string_arg :: String -> Property
prop_string_arg argument =
  let args = [ (argument, oBindAddress, optBindAddress)
             ]
  in conjoin $
     map (checkOpt Just defaultOptions failTest (const (==?)) Just) args

-- | Test a few integer arguments (only one for now).
prop_numeric_arg :: Int -> Property
prop_numeric_arg argument =
  checkOpt (Just . show) defaultOptions
    failTest (const (==?)) (Just . fromIntegral)
    (argument, oPort 0, optPort)

-- | Test a few boolean arguments.
case_bool_arg :: Assertion
case_bool_arg =
  mapM_ (checkOpt (const Nothing) defaultOptions assertFailure
                  assertEqual id)
        [ (False, oNoDaemonize,  optDaemonize)
        , (True,  oDebug,        optDebug)
        , (True,  oNoUserChecks, optNoUserChecks)
        ]

-- | Tests a few invalid arguments.
case_wrong_arg :: Assertion
case_wrong_arg =
  mapM_ (passFailOpt defaultOptions assertFailure (return ()))
        [ (oPort 0,      "x",   "10")
        ]

-- | Test that the option list supports some common options.
case_stdopts :: Assertion
case_stdopts =
  checkEarlyExit defaultOptions "prog" [oShowHelp, oShowVer] []

testSuite "Daemon"
          [ 'prop_string_arg
          , 'prop_numeric_arg
          , 'case_bool_arg
          , 'case_wrong_arg
          , 'case_stdopts
          ]
