{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

{-| A validation monad and corresponding utilities

The monad allows code to emit errors during checking.

-}

module Idly.Utils.Validate
  ( ValidationMonadT()
  , ValidationMonad
  , report
  , reportIf
  , runValidate
  , runValidateT
  , execValidateT
  , execValidate
  , evalValidateT
  , evalValidate
  , Validatable(..)
  , validate'
  ) where

import Control.Applicative()
import Control.Arrow
import Control.Monad
import Control.Monad.Error
import Control.Monad.Writer
import qualified Data.Foldable as F
import Data.Functor.Identity
import Data.List (intercalate)
import Data.Sequence

-- | Monad for running validation checks.
newtype ValidationMonadT m a =
  ValidationMonad { runValidationMonad :: WriterT (Seq String) m a }
  deriving (Functor, Applicative, Monad)

type ValidationMonad = ValidationMonadT Identity

-- | An utility function that emits a single message into a validation monad.
report :: (Monad m) => String -> ValidationMonadT m ()
report = ValidationMonad . tell . singleton

-- | An utility function that conditionally emits a message into
-- a validation monad.
-- It's a combination of 'when' and 'report'.
reportIf :: (Monad m) => Bool -> String -> ValidationMonadT m ()
reportIf b = when b . report

-- | An utility function that runs a monadic validation action
-- and returns the list of errors.
runValidateT :: (Monad m) => ValidationMonadT m a -> m (a, [String])
runValidateT = liftM (second F.toList) . runWriterT . runValidationMonad

-- | An utility function that runs a monadic validation action
-- and returns the list of errors.
runValidate :: ValidationMonad a -> (a, [String])
runValidate = runIdentity . runValidateT

-- | An utility function that runs a monadic validation action
-- and returns the list of errors.
execValidateT :: (Monad m) => ValidationMonadT m () -> m [String]
execValidateT = liftM F.toList . execWriterT . runValidationMonad

-- | An utility function that runs a validation action
-- and returns the list of errors.
execValidate :: ValidationMonad () -> [String]
execValidate = runIdentity . execValidateT

-- | A helper function for throwing an exception if a list of errors
-- is non-empty.
throwIfErrors :: (MonadError e m, Error e) => (a, [String]) -> m a
throwIfErrors (x, []) = return x
throwIfErrors (_, es) = throwError (strMsg $ "Validation errors: "
                                             ++ intercalate "; " es)

-- | Runs a validation action and if there are errors, combine them
-- into an exception.
evalValidate :: (MonadError e m, Error e) => ValidationMonad a -> m a
evalValidate = throwIfErrors . runValidate

-- | Runs a validation action and if there are errors, combine them
-- into an exception.
evalValidateT :: (MonadError e m, Error e) => ValidationMonadT m a -> m a
evalValidateT k = runValidateT k >>= throwIfErrors

-- | A typeclass for objects that can be validated.
-- That is, they can perform an internal check and emit any
-- errors encountered.
-- Emiting no errors means the object is valid.
class Validatable a where
  validate :: a -> ValidationMonad ()

-- | Run validation and return the original value as well.
-- the original value.
validate' :: (Validatable a) => a -> ValidationMonad a
validate' x = x <$ validate x
