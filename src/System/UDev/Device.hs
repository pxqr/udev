module System.UDev.Device
       ( Device (..)
       , newFromSysPath

       , getDevNode
       , getParentWithSubsystemDevtype

       , getAction
       , getSysattrValue
       ) where

import Control.Applicative
import Data.ByteString as BS
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe

import System.UDev.Context
import System.UDev.Types



-- | Opaque object representing one kernel sys device.
newtype Device = Device { getDevice :: Ptr Device }

foreign import ccall unsafe "udev_device_ref"
  c_deviceRef :: Device -> IO Device

foreign import ccall unsafe "udev_device_unref"
  c_deviceUnref :: Device -> IO Device

instance Ref Device where
  ref   = c_deviceRef
  unref = c_deviceUnref

foreign import ccall unsafe "udev_device_get_udev"
  c_getUDev :: Device -> UDev

-- TODO
--instance UDevChild Device where
--  getUDev = c_getUDev

foreign import ccall unsafe "udev_device_new_from_syspath"
  c_newFromSysPath :: UDev -> CString -> IO Device

-- TODO type SysPath = FilePath
type SysPath = ByteString

-- | Create new udev device, and fill in information from the sys
-- device and the udev database entry. The syspath is the absolute
-- path to the device, including the sys mount point.
--
newFromSysPath :: UDev -> SysPath -> IO Device
newFromSysPath udev sysPath = do
  Device <$> (throwErrnoIfNull "newFromSysPath" $ do
    useAsCString sysPath $ \ c_sysPath -> do
      getDevice <$> c_newFromSysPath udev c_sysPath)


-- TODO rest

foreign import ccall unsafe "udev_device_get_devnode"
  c_getDevNode :: Device -> IO CString

getDevNode :: Device -> IO ByteString
getDevNode udev =
    packCString =<< throwErrnoIfNull "getDevNode" (c_getDevNode udev)

foreign import ccall unsafe "udev_device_get_parent_with_subsystem_devtype"
    c_getParentWithSubsystemDevtype :: Device -> CString -> CString
                                    -> IO Device

-- | Find the next parent device, with a matching subsystem and devtype
-- value, and fill in information from the sys device and the udev
-- database entry.
--
getParentWithSubsystemDevtype :: Device -> ByteString -> ByteString
                              -> IO (Maybe Device)
getParentWithSubsystemDevtype udev subsystem devtype = do
  mdev <- useAsCString subsystem $ \ c_subsystem ->
              useAsCString devtype $ \ c_devtype ->
                  c_getParentWithSubsystemDevtype udev c_subsystem c_devtype
  return $ if getDevice mdev == nullPtr then Nothing else Just mdev

foreign import ccall unsafe "udev_device_get_action"
  c_getAction :: Device -> CString

-- TODO data Action

-- | This is only valid if the device was received through a
-- monitor. Devices read from sys do not have an action string.
--
getAction :: Device -> Maybe ByteString
getAction dev
    | c_action == nullPtr = Nothing
    |      otherwise      = Just $ unsafePerformIO $ packCString c_action
  where
    c_action = c_getAction dev


foreign import ccall unsafe "udev_device_get_sysattr_value"
  c_getSysattrValue :: Device -> CString -> CString

-- | The retrieved value is cached in the device. Repeated calls will
-- return the same value and not open the attribute again.
--
getSysattrValue :: Device -> ByteString -> ByteString
getSysattrValue dev sysattr = do
  unsafePerformIO $ do
    packCString =<< useAsCString sysattr (return . c_getSysattrValue dev)
