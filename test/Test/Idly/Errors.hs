{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| Unittests for "Idly.Errors".

-}

module Test.Idly.Errors (testErrors) where

import           Test.QuickCheck

import           Test.Idly.TestCommon
import           Test.Idly.TestHelper

import qualified Idly.Errors          as Errors

$(genArbitrary ''Errors.ErrorCode)

$(genArbitrary ''Errors.IdlyException)

-- | Tests error serialisation.
prop_GenericError_serialisation :: Errors.IdlyException -> Property
prop_GenericError_serialisation = testSerialisation

testSuite "Errors"
          [ 'prop_GenericError_serialisation
          ]
