{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-| Functions of the metadata daemon exported for RPC

-}
module Idly.Metad.ConfigCore where

import           Control.Applicative            ()
import           Control.Concurrent.MVar.Lifted
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Language.Haskell.TH            (Name)
import qualified Text.JSON                      as J

import           Idly.BasicTypes
import           Idly.Errors
import qualified Idly.JSON                      as J
import           Idly.Logging                   as L
import           Idly.Metad.Types               (InstanceParams)

-- * The monad in which all the Metad functions execute

data MetadHandle = MetadHandle
  { mhInstParams :: MVar InstanceParams
  }

-- | A type alias for easier referring to the actual content of the monad
-- when implementing its instances.
type MetadMonadIntType = ReaderT MetadHandle IO

-- | The internal part of the monad without error handling.
newtype MetadMonadInt a = MetadMonadInt
  { getMetadMonadInt :: MetadMonadIntType a }
  deriving ( Functor, Applicative, Monad, MonadIO, MonadBase IO
           , L.MonadLog )

instance MonadBaseControl IO MetadMonadInt where
#if MIN_VERSION_monad_control(1,0,0)
-- Needs Undecidable instances
  type StM MetadMonadInt b = StM MetadMonadIntType b
  liftBaseWith f = MetadMonadInt . liftBaseWith
                   $ \r -> f (r . getMetadMonadInt)
  restoreM = MetadMonadInt . restoreM
#else
  newtype StM MetadMonadInt b = StMMetadMonadInt
    { runStMMetadMonadInt :: StM MetadMonadIntType b }
  liftBaseWith f = MetadMonadInt . liftBaseWith
                   $ \r -> f (liftM StMMetadMonadInt . r . getMetadMonadInt)
  restoreM = MetadMonadInt . restoreM . runStMMetadMonadInt
#endif

-- | Runs the internal part of the MetadMonad monad on a given daemon
-- handle.
runMetadMonadInt :: MetadMonadInt a -> MetadHandle -> IO a
runMetadMonadInt (MetadMonadInt k) = runReaderT k

-- | The complete monad with error handling.
type MetadMonad = ResultT IdlyException MetadMonadInt

-- * Basic functions in the monad

metadHandle :: MetadMonad MetadHandle
metadHandle = lift . MetadMonadInt $ ask

instParams :: MetadMonad InstanceParams
instParams = readMVar . mhInstParams =<< metadHandle

modifyInstParams :: (InstanceParams -> MetadMonad (InstanceParams, a))
                 -> MetadMonad a
modifyInstParams f = do
  h <- metadHandle
  modifyMVar (mhInstParams h) f
