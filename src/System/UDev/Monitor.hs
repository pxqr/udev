-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Connects to a device event source.
--
{-# LANGUAGE OverloadedStrings #-}
module System.UDev.Monitor
       ( Monitor

         -- * Creation
       , SourceId
       , udevId
       , kernelId
       , newFromNetlink

         -- * Receiving
       , enableReceiving
       , setReceiveBufferSize
       , getFd
       , getHandle
       , receiveDevice

         -- * Filter
       , filterAddMatchSubsystemDevtype
       , filterAddMatchTag
       , filterUpdate
       , filterRemove
       ) where

import Control.Applicative
import Control.Monad
import Data.ByteString as BS
import Foreign
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import System.Posix.Types
import System.Posix.IO
import System.IO

import System.UDev.Context
import System.UDev.Device
import System.UDev.Types

-- | Opaque object handling an event source.
newtype Monitor = Monitor { getMonitor :: Ptr Monitor }


foreign import ccall unsafe "udev_monitor_get_udev"
  c_getUDev :: Monitor -> UDev

instance UDevChild Monitor where
  getUDev = c_getUDev

foreign import ccall unsafe "udev_monitor_ref"
  c_ref :: Monitor -> IO Monitor

foreign import ccall unsafe "udev_monitor_unref"
  c_unref :: Monitor -> IO Monitor

instance Ref Monitor where
  ref   = c_ref
  unref = c_unref

foreign import ccall unsafe "udev_monitor_new_from_netlink"
  c_newFromNetlink :: UDev -> CString -> IO Monitor

newtype SourceId = SourceId ByteString

-- | Events are sent out after udev has finished its event processing,
-- all rules have been processed, and needed device nodes are created.
udevId :: SourceId
udevId = SourceId "udev"

kernelId :: SourceId
kernelId = SourceId "kernel"

-- | Create new udev monitor and connect to a specified event source.
newFromNetlink :: UDev -> SourceId -> IO Monitor
newFromNetlink udev (SourceId name) =
  Monitor <$> do
    throwErrnoIfNull "newFromNetlink" $ do
      useAsCString name $ \ c_name -> do
        getMonitor <$> c_newFromNetlink udev c_name


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
getFd :: Monitor -> IO Fd
getFd monitor = Fd <$> c_getFd monitor

-- | Retrieve the socket handle associated with the monitor.
getHandle :: Monitor -> IO Handle
getHandle = getFd >=> fdToHandle

foreign import ccall unsafe "udev_monitor_receive_device"
  c_receiveDevice :: Monitor -> IO Device

-- | Receive data from the udev monitor socket, allocate a new udev
-- device, fill in the received data, and return the device.
--
receiveDevice :: Monitor -> IO Device
receiveDevice monitor = do
  Device <$> do
    throwErrnoIfNull "receiveDevice" $ do
      getDevice <$> c_receiveDevice monitor

foreign import ccall unsafe "udev_monitor_filter_add_match_subsystem_devtype"
  c_filterAddMatchSubsystemDevtype :: Monitor -> CString -> CString -> IO CInt

-- | Filter events by subsystem and device type.
--
-- The filter /must be/ installed before the monitor is switched to
-- listening mode.
--
filterAddMatchSubsystemDevtype :: Monitor -> ByteString -> ByteString -> IO ()
filterAddMatchSubsystemDevtype monitor subsystem devtype = do
  throwErrnoIfMinus1_ "filterAddMatchSubsystemDevtype" $
    useAsCString subsystem $ \ c_subsystem ->
      useAsCString devtype $ \ c_devtype   ->
        c_filterAddMatchSubsystemDevtype monitor c_subsystem c_devtype

foreign import ccall unsafe "udev_monitor_filter_add_match_tag"
  c_filterAddMatchTag :: Monitor -> CString -> IO CInt

-- | The filter must be installed before the monitor is switched to
-- listening mode.
--
filterAddMatchTag :: Monitor -> ByteString -> IO ()
filterAddMatchTag monitor tag = do
  throwErrnoIfMinus1_ "filterAddMatchTag" $ do
    useAsCString tag $ \ c_tag -> do
      c_filterAddMatchTag monitor c_tag

foreign import ccall unsafe "udev_monitor_filter_update"
  c_filterUpdate :: Monitor -> IO CInt

-- | Update the installed socket filter. This is only needed, if the
-- filter was removed or changed.
--
filterUpdate :: Monitor -> IO ()
filterUpdate monitor = do
  throwErrnoIfMinus1_ "filterUpdate" $ do
    c_filterUpdate monitor


foreign import ccall unsafe "udev_monitor_filter_remove"
  c_filterRemove :: Monitor -> IO CInt

-- | Remove all filters from monitor.
filterRemove :: Monitor -> IO ()
filterRemove monitor = do
  throwErrnoIfMinus1_ "filterRemove" $ do
    c_filterRemove monitor
