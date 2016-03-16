{-| Utilities related to randomized computations.

-}

module Idly.Utils.Random
  ( generateSecret
  , generateOneMAC
  , delayRandom
  ) where

import           Control.Applicative ()
import           Control.Concurrent  (threadDelay)
import           Control.Monad
import           Control.Monad.State
import           System.Random
import           Text.Printf

-- | Generates a random secret of a given length.
-- The type is chosen so that it can be easily wrapped into a state monad.
generateSecret :: (RandomGen g) => Int -> g -> (String, g)
generateSecret n =
  runState . liftM (concatMap $ printf "%02x")
  $ replicateM n (state $ randomR (0 :: Int, 255))

-- | Given a prefix, randomly generates a full MAC address.
--
-- See 'generateMAC' for discussion about how this function uses
-- the random generator.
generateOneMAC :: (RandomGen g) => String -> g -> (String, g)
generateOneMAC prefix = runState $
  let randByte = state (randomR (0, 255 :: Int))
  in printf "%s:%02x:%02x:%02x" prefix <$> randByte <*> randByte <*> randByte

-- | Wait a time period randomly chosen within the given bounds
-- (in microseconds).
delayRandom :: (Int, Int) -> IO ()
delayRandom = threadDelay <=< randomRIO
