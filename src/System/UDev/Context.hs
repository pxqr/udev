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
       , freeUDev
       , withUDev

         -- * Logging
       , Priority (..)
       , Logger
       , getLogPriority
       , setLogPriority
       , setLogger
       , defaultLogger

         -- * User data
       , getUserdata
       , setUserdata
       ) where

import Control.Applicative
import Control.Monad (void)
import Control.Exception
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Foreign (Ptr, FunPtr)
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

freeUDev :: UDev -> IO ()
freeUDev = void . unref

-- | Like 'newUDev' but context will be released at exit.
withUDev :: (UDev -> IO a) -> IO a
withUDev = bracket newUDev freeUDev

{-----------------------------------------------------------------------
--  Logging
-----------------------------------------------------------------------}

-- | Log message priority.
data Priority = LogError -- ^ error conditions
              | LogInfo  -- ^ informational
              | LogDebug -- ^ debug-level messages
                deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Convert priority to priority code.
prioToNr :: Priority -> CInt
prioToNr LogError = 3
prioToNr LogInfo  = 6
prioToNr LogDebug = 7

-- | Convert priority code to priority.
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

-- | Logger function will called by udev on events.
type Logger  = UDev
            -> Priority   -- ^ message priority;
            -> ByteString -- ^ position: file
            -> Int        -- ^ position: line
            -> ByteString -- ^ position: function
            -> ByteString -- ^ message body
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

-- | Default logger will just print @%PRIO %FILE:%LINE:\n%FN: %FORMAT@
-- to stdout.
defaultLogger :: Logger
defaultLogger _ priority file line fn format =
  BC.putStrLn $ BS.concat
    [ BC.pack (show priority), " "
    , file, ":", BC.pack (show line), ":\n"
    , "  ", fn, ": ", format
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
