{-| Common tests for PartialParams instances

-}

module Test.Idly.PartialParams
  ( testFillParamsLaw1
  , testToParamsLaw2
  , testToFilledLaw3
  , testToFilledMonoidLaw1
  , testToFilledMonoidLaw2
  ) where

import Data.Monoid

import Test.QuickCheck

import Idly.PartialParams

import Test.Idly.TestCommon

-- | Checks for serialisation idempotence.
testFillParamsLaw1 :: (PartialParams f p, Show f, Eq f)
                   => f -> p -> Property
testFillParamsLaw1 f p = fillParams (fillParams f p) p ==? fillParams f p

-- | Tests that filling partial parameters satisfies the law.
testToParamsLaw2 :: (PartialParams f p, Show f, Eq f) => f -> f -> Property
testToParamsLaw2 x f = fillParams x (toPartial f) ==? f

-- | Tests that converting partial to filled parameters satisfies the law.
testToFilledLaw3 :: (PartialParams f p, Show f, Eq f) => f -> Property
testToFilledLaw3 f = toFilled (toPartial f) ==? Just f

-- | Tests that the partial params behave correctly as a monoid action.
testToFilledMonoidLaw1 :: (PartialParams f p, Show f, Eq f, Monoid p)
                       => f -> Property
testToFilledMonoidLaw1 f = fillParams f mempty ==? f

-- | Tests that the partial params behave correctly as a monoid action.
testToFilledMonoidLaw2 :: (PartialParams f p, Show f, Eq f, Monoid p)
                       => f -> p -> p -> Property
testToFilledMonoidLaw2 f p1 p2 =
  fillParams f (p1 <> p2) ==? fillParams (fillParams f p1) p2
