{-# LANGUAGE TemplateHaskell #-}
{-| Unittests for constants

-}

module Test.Idly.Constants (testConstants) where

import           Test.HUnit           (Assertion)
import qualified Test.HUnit           as HUnit

import qualified Idly.Constants       as Constants
import qualified Idly.ConstantUtils   as ConstantUtils
import qualified Test.Idly.TestHelper as TestHelper

{-# ANN module "HLint: ignore Use camelCase" #-}

case_buildVersion :: Assertion
case_buildVersion = do
  HUnit.assertEqual "Build version"
                    (ConstantUtils.buildVersion 0 0 0) 0
  HUnit.assertEqual "Build version"
                    (ConstantUtils.buildVersion 10 10 1010) 10101010
  HUnit.assertEqual "Build version"
                    (ConstantUtils.buildVersion 12 34 5678) 12345678
  HUnit.assertEqual "Build version"
                    (ConstantUtils.buildVersion 99 99 9999) 99999999

TestHelper.testSuite "Constants"
  [ 'case_buildVersion
  ]
