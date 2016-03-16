{-| Metadata daemon server, which controls the configuration and web servers.

-}
module Idly.Metad.Server (start) where

import           Control.Concurrent
import qualified Data.Map              (empty)

import           Idly.Daemon           (DaemonOptions)
import           Idly.Metad.ConfigCore (MetadHandle (..))
import qualified Idly.Metad.WebServer  as WebServer

start :: DaemonOptions -> IO ()
start opts =
  do config <- newMVar Data.Map.empty
     WebServer.start opts config
--     _ <- forkIO $ WebServer.start opts config
     return ()
