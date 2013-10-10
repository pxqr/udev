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
{-# LANGUAGE OverloadedStrings        #-}
module System.UDev.Context
       ( -- * Context
         UDev
       , UDevChild (..)
       , newUDev
       , withUDev

         -- * Logging
       , Priority (..)
       , getLogPriority
       , setLogPriority
       , setLogger
       , defaultLogger

         -- * User data
       , getUserdata
       , setUserdata
       ) where

import Control.Applicative
import Control.Exception
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Unsafe.Coerce

import System.UDev.Types


foreign import ccall unsafe "udev_new"
  c_new :: IO UDev

-- | Create udev library context. This reads the udev configuration
--   file, and fills in the default values.
--
newUDev :: IO UDev
newUDev = c_new

withUDev :: (UDev -> IO a) -> IO a
withUDev = bracket c_new unref

{-----------------------------------------------------------------------
--  Logging
-----------------------------------------------------------------------}

data Priority = LogError -- ^ error conditions
              | LogInfo  -- ^ informational
              | LogDebug -- ^ debug-level messages
                deriving (Show, Read, Eq, Ord, Enum, Bounded)

prioToNr :: Priority -> CInt
prioToNr LogError = 3
prioToNr LogInfo  = 6
prioToNr LogDebug = 7

nrToPrio :: CInt -> IO Priority
nrToPrio 3 = pure LogError
nrToPrio 6 = pure LogInfo
nrToPrio 7 = pure LogDebug
nrToPrio n = throwIO $ PatternMatchFail msg
  where
    msg = "unknown priority number: " ++ show n

foreign import ccall unsafe "udev_get_log_priority"
  c_getLogPriority :: UDev -> IO CInt

-- | The initial logging priority is read from the udev config file at
-- startup.
getLogPriority :: UDev -> IO Priority
getLogPriority udev = nrToPrio =<< c_getLogPriority udev

foreign import ccall unsafe "udev_set_log_priority"
  c_setLogPriority :: UDev -> CInt -> IO ()

-- | Set the current logging priority. The value controls which
-- messages are logged.
setLogPriority :: UDev -> Priority -> IO ()
setLogPriority udev prio = c_setLogPriority udev (prioToNr prio)

type CLogger = UDev -> CInt -> CString -> CInt -> CString -> CString -> IO ()
type Logger  = UDev -> Priority -> ByteString -> Int -> ByteString -> ByteString
            -> IO ()

marshLogger :: Logger -> CLogger
marshLogger logger udev c_priority c_file c_line c_fn c_format = do
  file   <- packCString c_file
  fn     <- packCString c_fn
  format <- packCString c_format
  prio   <- nrToPrio    c_priority
  logger udev prio file (fromIntegral c_line) fn format

foreign import ccall "wrapper"
  mkLogger :: CLogger -> IO (FunPtr CLogger)

foreign import ccall "udev_set_log_fn"
  c_setLogger :: UDev -> FunPtr CLogger -> IO ()

-- | The built-in logging writes to stderr. It can be overridden by a
-- custom function, to plug log messages into the users' logging
-- functionality.
setLogger :: UDev -> Logger -> IO ()
setLogger udev logger = c_setLogger udev =<< mkLogger (marshLogger logger)


defaultLogger :: UDev -> Priority   -> ByteString
              -> Int  -> ByteString -> ByteString
              -> IO ()
defaultLogger _ priority file line fn format = do
  BC.putStrLn $ BS.concat
    [ BC.pack (show priority), " "
    , file, ":", BC.pack (show line), ":", fn, ": "
    , format
    ]

{-----------------------------------------------------------------------
--  Userdata
-----------------------------------------------------------------------}

foreign import ccall unsafe "udev_get_userdata"
  c_getUserdata :: UDev -> IO (Ptr ())

-- | Retrieve stored data pointer from library context. This might be
-- useful to access from callbacks like a custom logging function.
--
getUserdata :: UDev -> IO a
getUserdata udev = unsafeCoerce <$> c_getUserdata udev

foreign import ccall unsafe "udev_set_userdata"
  c_setUserdata :: UDev -> Ptr () -> IO ()

-- | Store custom userdata in the library context.
setUserdata :: UDev -> a -> IO ()
setUserdata udev ud = c_setUserdata udev (unsafeCoerce ud)