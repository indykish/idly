{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-| Web server for the metadata daemon.

-}

module Idly.Metad.WebServer (start) where

import Control.Applicative
import Control.Concurrent (MVar, readMVar)
import Control.Monad.Error.Class (MonadError, catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.CatchIO as CatchIO (catch)
import qualified Data.CaseInsensitive as CI
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as ByteString (pack, unpack)
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Text.JSON (JSValue, Result(..), JSObject)
import qualified Text.JSON as JSON
import System.FilePath ((</>))

import Idly.Daemon
import qualified Idly.Constants as Constants
import qualified Idly.Logging as Logging
import Idly.Runtime (IdlyDaemon(..), ExtraLogReason(..))
import qualified Idly.Runtime as Runtime

import Idly.Metad.Types (InstanceParams)

type MetaM = Snap ()

split :: String -> [String]
split str =
  case span (/= '/') str of
    (x, []) -> [x]
    (x, _:xs) -> x:split xs

lookupInstanceParams :: MonadError String m => String -> Map String b -> m b
lookupInstanceParams inst params =
  case Map.lookup inst params of
    Nothing -> throwError $ "Could not get instance params for " ++ show inst
    Just x -> return x

-- | The 404 "not found" error.
error404 :: MetaM
error404 = do
  modifyResponse $ setResponseStatus 404 "Not found"
  writeBS "Resource not found"

-- | The 405 "method not allowed error", including the list of allowed methods.
error405 :: [Method] -> MetaM
error405 ms = modifyResponse $
  addHeader (CI.mk "Allow") (ByteString.pack . intercalate ", " $ map show ms)
  . setResponseStatus 405 "Method not allowed"

maybeResult :: MonadError String m => Result t -> (t -> m a) -> m a
maybeResult (Error err) _ = throwError err
maybeResult (Ok x) f = f x


handleMetadata _ GET  "idly" "latest" "read" =
  liftIO $ Logging.logInfo "idly READ"
handleMetadata _ _  "idly" "latest" "read" =
  error405 [GET]
handleMetadata _ POST "idly" "latest" "write" =
  liftIO $ Logging.logInfo "idly WRITE"
handleMetadata _ _ "idly" "latest" "write" =
  error405 [POST]
handleMetadata _ _ _ _ _ =
  error404

routeMetadata :: MVar InstanceParams -> MetaM
routeMetadata params =
  route [ (providerRoute1, dispatchMetadata)
        , (providerRoute2, dispatchMetadata)
        ] <|> dispatchMetadata
  where provider = "provider"
        version  = "version"

        providerRoute1 = ByteString.pack $ ':':provider ++ "/" ++ ':':version
        providerRoute2 = ByteString.pack $ ':':version

        getParamString :: String -> Snap String
        getParamString =
          fmap (maybe "" ByteString.unpack) . getParam . ByteString.pack

        dispatchMetadata =
          do m <- rqMethod <$> getRequest
             p <- getParamString provider
             v <- getParamString version
             r <- ByteString.unpack . rqPathInfo <$> getRequest
             handleMetadata params m p v r

defaultHttpConf :: DaemonOptions -> FilePath -> FilePath -> Config Snap ()
defaultHttpConf opts accessLog errorLog =
  maybe id (setBind . ByteString.pack) (optBindAddress opts) .
  setAccessLog (ConfigFileLog accessLog) .
  setCompression False .
  setErrorLog (ConfigFileLog errorLog) .
  setPort (maybe Constants.defaultMetadPort fromIntegral (optPort opts)) .
  setVerbose False $
  emptyConfig

start :: DaemonOptions -> MVar InstanceParams -> IO ()
start opts params = do
  accessLog <- Runtime.daemonsExtraLogFile IdlyMetad AccessLog
  errorLog <- Runtime.daemonsExtraLogFile IdlyMetad ErrorLog
  httpServe (defaultHttpConf opts accessLog errorLog) (routeMetadata params)
