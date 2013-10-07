-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   The context contains the default values read from the udev config
--   file, and is passed to all library operations.
--
{-# LANGUAGE ForeignFunctionInterface #-}
module System.UDev.Context
       ( UDev (..)
       , UDevChild (..)

       , newUDev
       , withUDev

         -- * Logging
       , getLogPriority
       , setLogPriority
       , setLogger
       ) where

import Control.Applicative
import Control.Exception
import Data.ByteString as BS
import Foreign
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types

import System.UDev.Types


-- | Opaque object representing the library context.
newtype UDev = UDev (Ptr UDev)

class UDevChild a where
  getUDev :: a -> UDev

instance UDevChild UDev where
  getUDev = id

foreign import ccall unsafe "udev_ref"
  c_ref :: UDev -> IO UDev

foreign import ccall unsafe "udev_unref"
  c_unref :: UDev -> IO UDev

instance Ref UDev where
  ref   = c_ref
  unref = c_unref

foreign import ccall unsafe "udev_new"
  c_new :: IO UDev

-- | Create udev library context. This reads the udev configuration
--   file, and fills in the default values.
--
newUDev :: IO UDev
newUDev = c_new

withUDev :: (UDev -> IO a) -> IO a
withUDev = bracket c_new c_unref

{-----------------------------------------------------------------------
--  Logging
-----------------------------------------------------------------------}

-- TODO data Priority

foreign import ccall unsafe "udev_get_log_priority"
  c_getLogPriority :: UDev -> IO CInt

-- | The initial logging priority is read from the udev config file at
-- startup.
getLogPriority :: UDev -> IO Int
getLogPriority udev = fromIntegral <$> c_getLogPriority udev

foreign import ccall unsafe "udev_set_log_priority"
  c_setLogPriority :: UDev -> CInt -> IO ()

-- | Set the current logging priority. The value controls which
-- messages are logged.
setLogPriority :: UDev -> Int -> IO ()
setLogPriority udev prio = c_setLogPriority udev (fromIntegral prio)

type CLogger = UDev -> CInt -> CString -> CInt -> CString -> CString -> IO ()
type Logger  = UDev -> Int -> ByteString -> Int -> ByteString -> ByteString
            -> IO ()

marshLogger :: Logger -> CLogger
marshLogger logger udev c_priority c_file c_line c_fn c_format = do
  file   <- packCString c_file
  fn     <- packCString c_fn
  format <- packCString c_format
  logger udev (fromIntegral c_priority) file (fromIntegral c_line) fn format

foreign import ccall "wrapper"
  mkLogger :: CLogger -> IO (FunPtr CLogger)

foreign import ccall "udev_set_log_fn"
  c_setLogger :: UDev -> FunPtr CLogger -> IO ()

-- | The built-in logging writes to stderr. It can be overridden by a
-- custom function, to plug log messages into the users' logging
-- functionality.
setLogger :: UDev -> Logger -> IO ()
setLogger udev logger = c_setLogger udev =<< mkLogger (marshLogger logger)
