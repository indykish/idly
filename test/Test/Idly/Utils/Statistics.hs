{-# LANGUAGE TemplateHaskell #-}

{-| Unit tests for Idly statistics utils.

-}

module Test.Idly.Utils.Statistics (testUtils_Statistics) where

import           Test.QuickCheck       (Property, choose, forAll, vectorOf)

import           Test.Idly.TestCommon
import           Test.Idly.TestHelper

import           Idly.Utils            (stdDev)
import           Idly.Utils.Statistics

-- | Test the update function for standard deviations against the naive
-- implementation.
prop_stddev_update :: Property
prop_stddev_update =
  forAll (choose (0, 6) >>= flip vectorOf (choose (0, 1))) $ \xs ->
  forAll (choose (0, 1)) $ \a ->
  forAll (choose (0, 1)) $ \b ->
  forAll (choose (1, 6) >>= flip vectorOf (choose (0, 1))) $ \ys ->
  let original = xs ++ [a] ++ ys
      modified = xs ++ [b] ++ ys
      with_update = getStatisticValue
                    $ updateStatistics (getStdDevStatistics original) (a,b)
      direct = stdDev modified
  in counterexample ("Value computed by update " ++ show with_update
                     ++ " differs too much from correct value " ++ show direct)
                    (abs (with_update - direct) < 1e-10)

testSuite "Utils/Statistics"
  [ 'prop_stddev_update
  ]
