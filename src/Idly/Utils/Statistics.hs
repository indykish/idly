{-# LANGUAGE BangPatterns #-}


{-| Utility functions for statistical accumulation. -}

module Idly.Utils.Statistics
  ( Statistics
  , getSumStatistics
  , getStdDevStatistics
  , getStatisticValue
  , updateStatistics
  ) where

import           Data.List (foldl')

-- | Abstract type of statistical accumulations. They behave as if the given
-- statistics were computed on the list of values, but they allow a potentially
-- more efficient update of a given value.
data Statistics = SumStatistics Double
                | StdDevStatistics Double Double Double deriving Show
                  -- count, sum, and not the sum of squares---instead the
                  -- computed variance for better precission.

-- | Get a statistics that sums up the values.
getSumStatistics :: [Double] -> Statistics
getSumStatistics = SumStatistics . sum

-- | Get a statistics for the standard deviation.
getStdDevStatistics :: [Double] -> Statistics
getStdDevStatistics xs =
  let (nt, st) = foldl' (\(n, s) x ->
                            let !n' = n + 1
                                !s' = s + x
                            in (n', s'))
                 (0, 0) xs
      mean = st / nt
      nvar = foldl' (\v x -> let d = x - mean in v + d * d) 0 xs
  in StdDevStatistics nt st (nvar / nt)

-- | Obtain the value of a statistics.
getStatisticValue :: Statistics -> Double
getStatisticValue (SumStatistics s) = s
getStatisticValue (StdDevStatistics _ _ var) = sqrt var

-- | In a given statistics replace on value by another. This
-- will only give meaningful results, if the original value
-- was actually part of the statistics.
updateStatistics :: Statistics -> (Double, Double) -> Statistics
updateStatistics (SumStatistics s) (x, y) = SumStatistics $ s +  (y - x)
updateStatistics (StdDevStatistics n s var) (x, y) =
  let !ds = y - x
      !dss = y * y - x * x
      !dnnvar = (n * dss - 2 * s * ds) - ds * ds
      !s' = s + ds
      !var' = max 0 $ var + dnnvar / (n * n)
  in StdDevStatistics n s' var'
