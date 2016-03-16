{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

{-| Provides all lens-related functions.

-}
module Idly.Lens
  ( module Control.Lens
  , lensWith
  , makeCustomLenses
  , makeCustomLenses'
  , traverseOf2
  , mapMOf2
  , atSet
  ) where

import           Control.Applicative  (WrappedMonad (..))
import           Control.Lens
import           Control.Monad
import           Data.Functor.Compose (Compose (..))
import qualified Data.Set             as S
import           Language.Haskell.TH

-- | Creates an optimized lens where the setter also gets the original value
-- from the getter.
lensWith :: (s -> a) -> (s -> a -> b -> t) -> Lens s t a b
lensWith sa sbt f s = uncurry (sbt s) <$> (\a -> fmap ((,) a) (f a)) (sa s)

lensFieldName :: String -> String
lensFieldName = (++ "L")

-- | Internal helper method for constructing partial set of lenses.
makeCustomLensesFiltered :: (String -> Bool) -> Name -> Q [Dec]
makeCustomLensesFiltered f = makeLensesWith customRules
  where
    customRules :: LensRules
    customRules = set lensField nameFun lensRules
#if MIN_VERSION_lens(4,5,0)
    nameFun :: Name -> [Name] -> Name -> [DefName]
    nameFun _ _ = liftM (TopName . mkName) . nameFilter . nameBase
#elif MIN_VERSION_lens(4,4,0)
    nameFun :: [Name] -> Name -> [DefName]
    nameFun _ = liftM (TopName . mkName) . nameFilter . nameBase
#else
    nameFun :: String -> Maybe String
    nameFun = nameFilter
#endif
    nameFilter :: (MonadPlus m) => String -> m String
    nameFilter = liftM lensFieldName . mfilter f . return

-- | Create lenses for all fields of a given data type.
makeCustomLenses :: Name -> Q [Dec]
makeCustomLenses = makeCustomLensesFiltered (const True)

-- | Create lenses for some fields of a given data type.
makeCustomLenses' :: Name -> [Name] -> Q [Dec]
makeCustomLenses' name lst = makeCustomLensesFiltered f name
  where
    allowed = S.fromList . map nameBase $ lst
    f = flip S.member allowed

-- | Traverses over a composition of two functors.
-- Most often the @g@ functor is @(,) r@ and 'traverseOf2' is used to
-- traverse an effectful computation that also returns an additional output
-- value.
traverseOf2 :: Over (->) (Compose f g) s t a b
            -> (a -> f (g b)) -> s -> f (g t)
traverseOf2 k f = getCompose . traverseOf k (Compose . f)

-- | Traverses over a composition of a monad and a functor.
-- See 'traverseOf2'.
mapMOf2 :: Over (->) (Compose (WrappedMonad m) g) s t a b
        -> (a -> m (g b)) -> s -> m (g t)
mapMOf2 k f = unwrapMonad . traverseOf2 k (WrapMonad . f)

-- | A helper lens over sets.
-- While a similar lens exists in the package (as @Lens' Set (Maybe ())@),
-- it's available only in most recent versions.
-- And using @Bool@ instead of @Maybe ()@ is more convenient.
atSet :: (Ord a) => a -> Lens' (S.Set a) Bool
atSet k = lensWith (S.member k) f
  where
    f s True False = S.delete k s
    f s False True = S.insert k s
    f s _ _        = s
