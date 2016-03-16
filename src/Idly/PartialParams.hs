{-# LANGUAGE FunctionalDependencies #-}

{-| Common functions for partial parameters -}


module Idly.PartialParams
  ( PartialParams(..)
  , isComplete
  ) where

import Data.Maybe (isJust)

-- | Represents that data type @p@ provides partial values for
-- data type @f@.
--
-- Note: To avoid needless type annotations, the functional dependencies
-- currently include @f -> p@. However, in theory it'd be possible for one
-- filled data type to have several partially filled ones.
--
-- Laws:
--
-- 1. @fillParams (fillParams f p) p = fillParams f p@.
-- 2. @fillParams _ (toPartial x) = x@.
-- 3. @toFilled (toPartial x) = Just x@.
--
-- If @p@ is also a 'Monoid' (or just 'Semigroup'), 'fillParams' is a monoid
-- (semigroup) action on @f@, therefore it should additionally satisfy:
--
-- - @fillParams f mempty = f@
-- - @fillParams f (p1 <> p2) = fillParams (fillParams f p1) p2@
class PartialParams f p | p -> f, f -> p where
  -- | Fill @f@ with any data that are set in @p@.
  -- Leave other parts of @f@ unchanged.
  fillParams :: f -> p -> f
  -- | Fill all fields of @p@ from @f@.
  toPartial :: f -> p
  -- | If all fields of @p@ are filled, convert it into @f@.
  toFilled :: p -> Maybe f

-- | Returns 'True' if a given partial parameters are complete.
-- See 'toFilled'.
isComplete :: (PartialParams f p) => p -> Bool
isComplete = isJust . toFilled
