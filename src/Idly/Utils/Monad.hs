{-| Utility functions for MonadPlus operations

-}

module Idly.Utils.Monad
  ( mretryN
  , retryMaybeN
  , anyM
  , allM
  , orM
  , unfoldrM
  , unfoldrM'
  , retryErrorN
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Maybe

-- | Retries the given action up to @n@ times.
-- The action signals failure by 'mzero'.
mretryN :: (MonadPlus m) => Int -> (Int -> m a) -> m a
mretryN n = msum . flip map [1..n]

-- | Retries the given action up to @n@ times.
-- The action signals failure by 'mzero'.
retryMaybeN :: (Monad m) => Int -> (Int -> MaybeT m a) -> m (Maybe a)
retryMaybeN = (runMaybeT .) . mretryN

-- | Retries the given action up to @n@ times until it succeeds.
-- If all actions fail, the error of the last one is returned.
-- The action is always run at least once, even if @n@ is less than 1.
retryErrorN :: (MonadError e m) => Int -> (Int -> m a) -> m a
retryErrorN n f = loop 1
  where
    loop i | i < n      = catchError (f i) (const $ loop (i + 1))
           | otherwise  = f i

-- * From monad-loops (until we can / want to depend on it):

-- | Short-circuit 'any' with a monadic predicate.
anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM p = foldM (\v x -> if v then return True else p x) False

-- | Short-circuit 'all' with a monadic predicate.
allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM p = foldM (\v x -> if v then p x else return False) True

-- | Short-circuit 'or' for values of type Monad m => m Bool
orM :: (Monad m) => [m Bool] -> m Bool
orM = anyM id

-- |See 'Data.List.unfoldr'.  This is a monad-friendly version of that.
unfoldrM :: (Monad m) => (a -> m (Maybe (b,a))) -> a -> m [b]
unfoldrM = unfoldrM'

-- | See 'Data.List.unfoldr'. This is a monad-friendly version of that, with a
-- twist. Rather than returning a list, it returns any MonadPlus type of your
-- choice.
unfoldrM' :: (Monad m, MonadPlus f) => (a -> m (Maybe (b,a))) -> a -> m (f b)
unfoldrM' f z = do
        x <- f z
        case x of
                Nothing         -> return mzero
                Just (x', z')   -> do
                        xs <- unfoldrM' f z'
                        return (return x' `mplus` xs)
