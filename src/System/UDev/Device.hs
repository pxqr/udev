-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
-- Representation of kernel sys devices. Devices are uniquely
-- identified by their syspath, every device has exactly one path in
-- the kernel sys filesystem. Devices usually belong to a kernel
-- subsystem, and have a unique name inside that subsystem.
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module System.UDev.Device
       ( Device
       , Devnum
       , Action (..)

         -- * Create
       , newFromSysPath
       , newFromDevnum
       , newFromSubsystemSysname
       , newFromDeviceId
       , newFromEnvironment

       , getParent
       , getParentWithSubsystemDevtype

         -- * Query
       , getDevpath
       , getSubsystem
       , getDevtype
       , getSyspath
       , getSysname
       , getSysnum
       , getDevnode
       , isInitialized
       , getDevlinksListEntry
       , getPropertiesListEntry
       , getTagsListEntry
       , getPropertyValue
       , getDriver
       , getDevnum
       , getAction

         -- * Sysattrs
       , getSysattrValue
       , setSysattrValue
       , getSysattrListEntry

         -- * Misc
       , getSeqnum
       , getUsecSinceInitialized
       , hasTag
       ) where

import Control.Applicative
import Data.Bits
import Data.ByteString as BS
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe
import System.Posix.FilePath

import System.UDev.Context
import System.UDev.List
import System.UDev.Types


foreign import ccall unsafe "udev_device_new_from_syspath"
  c_newFromSysPath :: UDev -> CString -> IO Device


-- | Create new udev device, and fill in information from the sys
-- device and the udev database entry. The syspath is the absolute
-- path to the device, including the sys mount point.
--
newFromSysPath :: UDev -> RawFilePath -> IO Device
newFromSysPath udev sysPath =
  Device <$> throwErrnoIfNull "newFromSysPath"
    (useAsCString sysPath $ \ c_sysPath ->
      getDevice <$> c_newFromSysPath udev c_sysPath)

type Dev_t = CULong

foreign import ccall unsafe "udev_device_new_from_devnum"
  c_newFromDevnum :: UDev -> CChar -> Dev_t -> IO Device

-- | Device number.
data Devnum = Devnum
  { major :: {-# UNPACK #-} !Int
  , minor :: {-# UNPACK #-} !Int
  } deriving (Show, Read, Eq, Ord)

nrToDevnum :: Dev_t -> Devnum
nrToDevnum x = Devnum
  { major = fromIntegral ((x `unsafeShiftR` 8) .&. 0xff)
  , minor = fromIntegral (x .&. 0xff)
  }
{-# INLINE nrToDevnum #-}

devnumToNr :: Devnum -> Dev_t
devnumToNr Devnum {..}
    = fromIntegral (((major .&. 0xff) `unsafeShiftL` 8)
                 .|. (minor .&. 0xff))
{-# INLINE devnumToNr #-}

-- | Create new udev device, and fill in information from the sys
-- device and the udev database entry. The device is looked-up by its
-- major/minor number and type. Character and block device numbers are
-- not unique across the two types.
--
newFromDevnum :: UDev -> Char -> Devnum -> IO Device
newFromDevnum udev char devnum
  = c_newFromDevnum udev (toEnum (fromEnum char)) (devnumToNr devnum)
{-# INLINE newFromDevnum #-}

foreign import ccall unsafe "udev_device_new_from_subsystem_sysname"
  c_newFromSubsystemSysname :: UDev -> CString -> CString -> IO Device

-- | The device is looked up by the subsystem and name string of the
-- device, like \"mem\" \/ \"zero\", or \"block\" \/ \"sda\".
--
newFromSubsystemSysname :: UDev -> ByteString -> ByteString -> IO Device
newFromSubsystemSysname udev subsystem sysname =
  useAsCString subsystem $ \ c_subsystem ->
    useAsCString sysname $ \ c_sysname   ->
      c_newFromSubsystemSysname udev c_subsystem c_sysname

foreign import ccall unsafe "udev_device_new_from_device_id"
  c_newFromDeviceId :: UDev -> CString -> IO Device

-- | The device is looked-up by a special string:
--
--     * b8:2 - block device major:minor
--
--     * c128:1 - char device major:minor
--
--     * n3 - network device ifindex
--
--     * +sound:card29 - kernel driver core subsystem:device name
--
newFromDeviceId :: UDev -> ByteString -> IO Device
newFromDeviceId udev devId =
  useAsCString devId $ \ c_devId ->
    c_newFromDeviceId udev c_devId

foreign import ccall unsafe "udev_device_new_from_environment"
  c_newFromEnvironment :: UDev -> IO Device

-- | Create new udev device, and fill in information from the current
-- process environment. This only works reliable if the process is
-- called from a udev rule. It is usually used for tools executed from
-- @IMPORT=@ rules.
--
newFromEnvironment :: UDev -> IO Device
newFromEnvironment = c_newFromEnvironment

foreign import ccall unsafe "udev_device_get_parent"
  c_getParent :: Device -> IO Device

--  TODO: [MEM]: The returned the device is not referenced. It is
-- attached to the child device, and will be cleaned up when the child
-- device is cleaned up.

-- | Find the next parent device, and fill in information from the sys
-- device and the udev database entry.
getParent :: Device -> IO Device
getParent = c_getParent

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

foreign import ccall unsafe "udev_device_get_devpath"
  c_getDevpath :: Device -> IO CString

-- TODO use RawFilePath

{-----------------------------------------------------------------------
--  Query
-----------------------------------------------------------------------}

-- | Retrieve the kernel devpath value of the udev device. The path
-- does not contain the sys mount point, and starts with a \'/\'.
--
getDevpath :: Device -> RawFilePath
getDevpath dev = unsafePerformIO $
  packCString =<< c_getDevpath dev

foreign import ccall unsafe "udev_device_get_subsystem"
  c_getSubsystem :: Device -> IO CString

packCStringMaybe :: CString -> IO (Maybe ByteString)
packCStringMaybe cstring =
  if cstring == nullPtr
  then return Nothing
  else Just <$> packCString cstring

-- | Retrieve the subsystem string of the udev device. The string does
-- not contain any \"/\".
--
getSubsystem :: Device -> Maybe ByteString
getSubsystem dev = unsafePerformIO $ packCStringMaybe =<< c_getSubsystem dev

foreign import ccall unsafe "udev_device_get_devtype"
  c_getDevtype :: Device -> IO CString

-- | Retrieve the devtype string of the udev device.
getDevtype :: Device -> Maybe ByteString
getDevtype dev = unsafePerformIO $ packCStringMaybe =<< c_getDevtype dev

foreign import ccall unsafe "udev_device_get_syspath"
  c_getSyspath :: Device -> IO CString

-- | Retrieve the sys path of the udev device. The path is an absolute
-- path and starts with the sys mount point.
--
getSyspath :: Device -> RawFilePath
getSyspath dev = unsafePerformIO $ packCString =<< c_getSyspath dev

foreign import ccall unsafe "udev_device_get_sysname"
  c_getSysname :: Device -> IO CString

-- | Get the kernel device name in /sys.
getSysname :: Device -> ByteString
getSysname dev = unsafePerformIO $ packCString =<< c_getSysname dev

foreign import ccall unsafe "udev_device_get_sysnum"
  c_getSysnum :: Device -> IO CString

--  TODO :: Device -> Maybe Int ?

-- | Get the instance number of the device.
getSysnum :: Device -> Maybe ByteString
getSysnum dev = unsafePerformIO $ packCStringMaybe =<< c_getSysnum dev

foreign import ccall unsafe "udev_device_get_devnode"
  c_getDevnode :: Device -> IO CString

-- | Retrieve the device node file name belonging to the udev
-- device. The path is an absolute path, and starts with the device
-- directory.
--
getDevnode :: Device -> Maybe ByteString
getDevnode udev = unsafePerformIO $ packCStringMaybe =<< c_getDevnode udev

foreign import ccall unsafe "udev_device_get_is_initialized"
  c_isInitialized :: Device -> IO CInt

-- | Check if udev has already handled the device and has set up
-- device node permissions and context, or has renamed a network
-- device.
--
-- This is only implemented for devices with a device node or network
-- interfaces. All other devices return 1 here.
--
isInitialized :: Device -> IO Bool
isInitialized dev = (< 0) <$> c_isInitialized dev

foreign import ccall unsafe "udev_device_get_devlinks_list_entry"
  c_getDevlinksListEntry :: Device -> IO List

-- | Retrieve the list of device links pointing to the device file of
-- the udev device. The next list entry can be retrieved with
-- 'getNext', which returns 'Nothing' if no more entries exist. The
-- devlink path can be retrieved from the list entry by 'getName'. The
-- path is an absolute path, and starts with the device directory.
--
getDevlinksListEntry :: Device -> IO List
getDevlinksListEntry = c_getDevlinksListEntry
{-# INLINE getDevlinksListEntry #-}

foreign import ccall unsafe "udev_device_get_properties_list_entry"
  c_getPropertiesListEntry :: Device -> IO List

-- | Retrieve the list of key/value device properties of the udev
-- device. The next list entry can be retrieved with 'getNext', which
-- returns 'Nothing' if no more entries exist. The property name can
-- be retrieved from the list entry by 'getName', the property value
-- by 'getValue'.
--
getPropertiesListEntry :: Device -> IO List
getPropertiesListEntry = c_getPropertiesListEntry
{-# INLINE getPropertiesListEntry #-}

foreign import ccall unsafe "udev_device_get_tags_list_entry"
  c_getTagsListEntry :: Device -> IO List

-- | Retrieve the list of tags attached to the udev device. The next
-- list entry can be retrieved with 'getNext', which returns 'Nothing'
-- if no more entries exist. The tag string can be retrieved from the
-- list entry by 'getName'.
--
getTagsListEntry :: Device -> IO List
getTagsListEntry = c_getTagsListEntry
{-# INLINE getTagsListEntry #-}

foreign import ccall unsafe "udev_device_get_property_value"
  c_getPropertyValue :: Device -> CString -> IO CString

-- | Get the value of a given property.
getPropertyValue :: Device -> ByteString -> IO (Maybe ByteString)
getPropertyValue dev prop = do
  res <- useAsCString prop $ \ c_prop ->
    c_getPropertyValue dev c_prop
  if res == nullPtr then return Nothing else Just <$> packCString res

foreign import ccall unsafe "udev_device_get_driver"
  c_getDriver :: Device -> IO CString

-- | Get the kernel driver name.
getDriver :: Device -> ByteString
getDriver dev = unsafePerformIO $
  packCString =<< c_getDriver dev

foreign import ccall unsafe "udev_device_get_devnum"
  c_getDevnum :: Device -> Dev_t

-- | Get the device major/minor number.
getDevnum :: Device -> Devnum
getDevnum = nrToDevnum . c_getDevnum
{-# INLINE getDevnum #-}

foreign import ccall unsafe "udev_device_get_action"
  c_getAction :: Device -> CString

data Action = Add
            | Remove
            | Change
            | Online
            | Offline
            | Other ByteString
              deriving (Show, Read, Eq, Ord)

marshalAction :: ByteString -> Action
marshalAction "add"     = Add
marshalAction "remove"  = Remove
marshalAction "change"  = Remove
marshalAction "online"  = Online
marshalAction "offline" = Offline
marshalAction   action  = Other action

-- | This is only valid if the device was received through a
-- monitor. Devices read from sys do not have an action string.
--
getAction :: Device -> Maybe Action
getAction dev
    | c_action == nullPtr = Nothing
    |      otherwise      = Just $ unsafePerformIO $
      marshalAction <$> packCString c_action
  where
    c_action = c_getAction dev


foreign import ccall unsafe "udev_device_get_sysattr_value"
  c_getSysattrValue :: Device -> CString -> CString

-- NOTE: getSysattrValue is not pure since we can alter some attr
-- using setSysattrValue

-- | The retrieved value is cached in the device. Repeated calls will
-- return the same value and not open the attribute again.
--
getSysattrValue :: Device -> ByteString -> IO ByteString
getSysattrValue dev sysattr =
    packCString =<< useAsCString sysattr (return . c_getSysattrValue dev)

foreign import ccall unsafe "udev_device_set_sysattr_value"
  c_setSysattrValue :: Device -> CString -> CString -> IO CInt

-- | Update the contents of the sys attribute and the cached value of
-- the device.
--
setSysattrValue :: Device
                -> ByteString -- ^ attribute name
                -> ByteString -- ^ new value to be set
                -> IO ()
setSysattrValue dev sysattr value =
  throwErrnoIf_ (0 <) "setSysattrValue" $
    useAsCString sysattr $ \ c_sysattr ->
      useAsCString value $ \ c_value   ->
        c_setSysattrValue dev c_sysattr c_value

foreign import ccall unsafe "udev_device_get_sysattr_list_entry"
  c_getSysAttrListEntry :: Device -> IO List

-- | Retrieve the list of available sysattrs, with value being empty;
-- This just return all available sysfs attributes for a particular
-- device without reading their values.
--
getSysattrListEntry :: Device -> IO List
getSysattrListEntry = c_getSysAttrListEntry
{-# INLINE getSysattrListEntry #-}

toMaybe :: CULLong -> Maybe Int
toMaybe 0 = Nothing
toMaybe n = Just (fromIntegral n)
{-# INLINE toMaybe #-}

foreign import ccall unsafe "udev_device_get_seqnum"
  c_getSeqnum :: Device -> IO CULLong

-- | This is only valid if the device was received through a
-- monitor. Devices read from sys do not have a sequence number.
--
getSeqnum :: Device -> IO (Maybe Int)
getSeqnum dev = toMaybe <$> c_getSeqnum dev
{-# INLINE getSeqnum #-}

foreign import ccall unsafe "udev_device_get_usec_since_initialized"
  c_getUsecSinceInitialized :: Device -> IO CULLong

-- | Return the number of microseconds passed since udev set up the
-- device for the first time.
--
--   This is only implemented for devices with need to store
--   properties in the udev database. All other devices return
--   'Nothing' here.
--
getUsecSinceInitialized :: Device -> IO (Maybe Int)
getUsecSinceInitialized dev = toMaybe <$> c_getUsecSinceInitialized dev

foreign import ccall unsafe "udev_device_has_tag"
  c_hasTag :: Device -> CString -> IO CInt

-- | Check if a given device has a certain tag associated.
hasTag :: Device -> ByteString -> IO Bool
hasTag dev tag =
  (1 ==) <$>
    useAsCString tag (\ c_tag ->
      c_hasTag dev c_tag)
