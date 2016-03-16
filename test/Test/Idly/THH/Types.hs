{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| Unittests for 'Idly.THH.Types'.

-}


module Test.Idly.THH.Types
  ( testTHH_Types
  ) where

import           Test.QuickCheck      as QuickCheck hiding (Result)
import qualified Text.JSON            as J

import           Test.Idly.TestCommon
import           Test.Idly.TestHelper

import           Idly.THH.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

-- * Instances

instance Arbitrary a => Arbitrary (OneTuple a) where
  arbitrary = fmap OneTuple arbitrary

-- * Properties

-- | Tests OneTuple serialisation.
prop_OneTuple_serialisation :: OneTuple String -> Property
prop_OneTuple_serialisation = testSerialisation

-- | Tests OneTuple doesn't deserialize wrong input.
prop_OneTuple_deserialisationFail :: Property
prop_OneTuple_deserialisationFail =
  conjoin . map (testDeserialisationFail (OneTuple "")) $
    [ J.JSArray []
    , J.JSArray [J.showJSON "a", J.showJSON "b"]
    , J.JSArray [J.showJSON (1 :: Int)]
    ]


-- * Test suite

testSuite "THH_Types"
  [ 'prop_OneTuple_serialisation
  , 'prop_OneTuple_deserialisationFail
  ]
