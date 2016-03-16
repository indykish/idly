{-| ConstantUtils contains the helper functions for constants

This module cannot be merged with 'Idly.Utils' because it would
create a circular dependency if imported, for example, from
'Idly.Constants'.

-}

module Idly.ConstantUtils where

import           Data.Char ()
import           Data.Set  (Set)
import qualified Data.Set  as Set (difference, fromList, toList, union)


-- | FrozenSet wraps a Haskell 'Set'
--
-- See 'PyValue' instance for 'FrozenSet'.
newtype FrozenSet a = FrozenSet { unFrozenSet :: Set a }
  deriving (Eq, Ord, Show)


mkSet :: Ord a => [a] -> FrozenSet a
mkSet = FrozenSet . Set.fromList

toList :: FrozenSet a -> [a]
toList = Set.toList . unFrozenSet

union :: Ord a => FrozenSet a -> FrozenSet a -> FrozenSet a
union x y = FrozenSet (unFrozenSet x `Set.union` unFrozenSet y)

difference :: Ord a => FrozenSet a -> FrozenSet a -> FrozenSet a
difference x y = FrozenSet (unFrozenSet x `Set.difference` unFrozenSet y)

-- | 'Protocol' represents the protocols used by the daemons
data Protocol = Tcp | Udp
  deriving (Show)


-- | Failure exit code
--
-- These are defined here and not in 'Idly.Constants' together with
-- the other exit codes in order to avoid a circular dependency
-- between 'Idly.Constants' and 'Idly.Runtime'
exitFailure :: Int
exitFailure = 1


-- | Console device
--
-- This is defined here and not in 'Idly.Constants' order to avoid a
-- circular dependency between 'Idly.Constants' and 'Idly.Logging'
devConsole :: String
devConsole = "/dev/console"

-- | Random uuid generator
--
-- This is defined here and not in 'Idly.Constants' order to avoid a
-- circular dependendy between 'Idly.Constants' and 'Idly.Types'
randomUuidFile :: String
randomUuidFile = "/proc/sys/kernel/random/uuid"


-- * Priority levels
--
-- This is defined here and not in 'Idly.Types' in order to avoid a
-- GHC stage restriction and because there is no suitable 'declareADT'
-- variant that handles integer values directly.

priorityLow :: Int
priorityLow = 10

priorityNormal :: Int
priorityNormal = 0

priorityHigh :: Int
priorityHigh = -10

-- | Calculates int version number from major, minor and revision
-- numbers.
buildVersion :: Int -> Int -> Int -> Int
buildVersion major minor revision =
  1000000 * major + 10000 * minor + 1 * revision
