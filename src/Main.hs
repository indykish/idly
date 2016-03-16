{-| Metadata daemon.

-}

module Main (main) where

import qualified Idly.Constants    as Constants
import           Idly.Daemon       (OptType)
import qualified Idly.Daemon       as Daemon
import qualified Idly.Metad.Server as Server
import qualified Idly.Runtime      as Runtime

options :: [OptType]
options =
  [ Daemon.oBindAddress
  , Daemon.oDebug
  , Daemon.oNoDaemonize
  , Daemon.oNoUserChecks
  , Daemon.oPort Constants.defaultMetadPort
  ]

main :: IO ()
main =
  Daemon.genericMain Runtime.IdlyMetad options
    (\_ -> return . Right $ ())
    (\_ _ -> return ())
    (\opts _ _ -> Server.start opts)
