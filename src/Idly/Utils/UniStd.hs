{-# LANGUAGE ForeignFunctionInterface #-}

{-| Necessary foreign function calls

...with foreign functions declared in unistd.h

-}

module Idly.Utils.UniStd
  ( fsyncFile
  ) where

import           Control.Exception  (bracket)
import           Foreign.C
import           System.Posix.IO
import           System.Posix.Types

import           Idly.BasicTypes

foreign import ccall "fsync" fsync :: CInt -> IO CInt

-- Opens a file and calls fsync(2) on the file descriptor.
--
-- Because of a bug in GHC 7.6.3 (at least), calling 'hIsClosed' on a handle
-- to get the file descriptor leaks memory. Therefore we open a given file
-- just to sync it and close it again.
fsyncFile :: (Error e) => FilePath -> ResultT e IO ()
fsyncFile path = liftIO
  $ bracket (openFd path ReadOnly Nothing defaultFileFlags) closeFd callfsync
  where
    callfsync (Fd fd) = throwErrnoPathIfMinus1_ "fsyncFile" path $ fsync fd
