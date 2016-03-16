{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| Unittests for mutli-maps

-}


module Test.Idly.Utils.MultiMap
  ( testUtils_MultiMap
  ) where

import           Control.Applicative
import qualified Data.Map             as M
import qualified Data.Set             as S

import           Test.QuickCheck

import           Test.Idly.TestCommon
import           Test.Idly.TestHelper

import           Idly.Utils.MultiMap  as MM

instance (Arbitrary k, Ord k, Arbitrary v, Ord v)
         => Arbitrary (MultiMap k v) where
  arbitrary = frequency
    [ (1, (multiMap . M.fromList)
          <$> listOf ((,) <$> arbitrary
                          <*> (S.fromList <$> listOf arbitrary)))
    , (4, MM.insert <$> arbitrary <*> arbitrary <*> arbitrary)
    , (1, MM.fromList <$> listOf ((,) <$> arbitrary <*> arbitrary))
    , (3, MM.delete <$> arbitrary <*> arbitrary <*> arbitrary)
    , (1, MM.deleteAll <$> arbitrary <*> arbitrary)
    ]

-- | A data type for testing extensional equality.
data Three = One | Two | Three
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Arbitrary Three where
  arbitrary = elements [minBound..maxBound]

-- | Tests the extensional equality of multi-maps.
prop_MultiMap_equality
  :: MultiMap Three Three -> MultiMap Three Three -> Property
prop_MultiMap_equality m1 m2 =
  let testKey k = MM.lookup k m1 == MM.lookup k m2
   in counterexample ("Extensional equality of '" ++ show m1
                      ++ "' and '" ++ show m2 ++ " doesn't match '=='.")
      $ all testKey [minBound..maxBound] ==? (m1 == m2)

prop_MultiMap_serialisation :: MultiMap Int Int -> Property
prop_MultiMap_serialisation = testSerialisation

testSuite "Utils/MultiMap"
  [ 'prop_MultiMap_equality
  , 'prop_MultiMap_serialisation
  ]
