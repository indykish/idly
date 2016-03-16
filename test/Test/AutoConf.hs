{-# LANGUAGE TemplateHaskell #-}
{-| Unittests for 'AutoConf'

-}

module Test.AutoConf where

import qualified Data.Char            as Char (isAlpha)
import           Test.HUnit           as HUnit

import qualified AutoConf             (localstatedir)
import qualified Test.Idly.TestHelper as TestHelper

{-# ANN module "HLint: ignore Use camelCase" #-}

-- | 'isFilePath x' tests whether @x@ is a valid filepath
--
-- A valid filepath must be absolute and must not contain commas.
isFilePath :: String -> Bool
isFilePath ('/':str) = ',' `notElem` str
isFilePath _ = False



case_localstatedir :: Assertion
case_localstatedir =
  HUnit.assertBool
    "'localstatedir' is invalid"
    (isFilePath AutoConf.localstatedir)




TestHelper.testSuite "AutoConf"
  [ 'case_localstatedir ]
