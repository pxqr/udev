module System.UDev.Monitor
       ( Monitor

       , enableReceiving
       , setReceiveBufferSize
       , getHandle

         -- * Filter
       , filterAddMatchSubsystemDevtype
       , filterAddMatchTag
       , filterUpdate
       , filterRemove
       ) where

import Control.Applicative
import Foreign
import Foreign.C.Error
import Foreign.C.Types
import System.Posix.Types
import System.Posix.IO
import System.IO

import System.UDev.Context
import System.UDev.Types


newtype Monitor = Monitor (Ptr Monitor)


foreign import ccall unsafe "udev_monitor_get_udev"
  c_getUDev :: Monitor -> UDev

--instance UDevChild UDev where
--  getUDev = c_getUDev

foreign import ccall unsafe "udev_monitor_ref"
  c_ref :: Monitor -> IO Monitor

foreign import ccall unsafe "udev_monitor_unref"
  c_unref :: Monitor -> IO Monitor

instance Ref Monitor where
  ref   = c_ref
  unref = c_unref

foreign import ccall unsafe "udev_monitor_enable_receiving"
  c_enableReceiving :: Monitor -> IO CInt

-- | Binds the udev_monitor socket to the event source.
enableReceiving :: Monitor -> IO ()
enableReceiving monitor = do
  throwErrnoIfMinus1_ "enableReceiving" $ do
    c_enableReceiving monitor

foreign import ccall unsafe "udev_monitor_set_receive_buffer_size"
  c_setReceiveBufferSize :: Monitor -> CInt -> IO CInt

-- | Set the size of the kernel socket buffer.
setReceiveBufferSize :: Monitor -> Int -> IO ()
setReceiveBufferSize monitor size = do
  throwErrnoIfMinus1_ "setReceiveBufferSize" $ do
    c_setReceiveBufferSize monitor (fromIntegral size)

foreign import ccall unsafe "udev_monitor_get_fd"
  c_getFd :: Monitor -> IO CInt

-- | Retrieve the socket file descriptor associated with the monitor.
getHandle :: Monitor -> IO Handle
getHandle monitor = do
  fd <- c_getFd monitor
  fdToHandle $ Fd fd

filterAddMatchSubsystemDevtype :: ()
filterAddMatchSubsystemDevtype = undefined

filterAddMatchTag :: Monitor -> IO ()
filterAddMatchTag = undefined

filterUpdate :: Monitor -> IO ()
filterUpdate = undefined

-- | Remove all filters from monitor.
foreign import ccall unsafe "udev_monitor_filter_remove"
  c_filterRemove :: Monitor -> IO ()

filterRemove :: Monitor -> IO ()
filterRemove = undefined