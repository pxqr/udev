-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   Lookup devices in the sys filesystem, filter devices by
--   properties, and return a sorted list of devices.
--
{-# LANGUAGE ForeignFunctionInterface #-}
module System.UDev.Enumerate
       ( Enumerate

       , newEnumerate

         -- * Match
       , Subsystem
       , addMatchSubsystem
       , addNoMatchSubsystem

       , SysAttr
       , SysValue
       , addMatchSysattr
       , addNoMatchSysattr

       , addMatchProperty
       , addMatchTag
       , addMatchParent
       , addMatchIsInitialized
       , addMatchSysname
       , addSyspath

         -- * Scan
       , scanDevices
       , scanSubsystems

         -- * Query
       , getListEntry
       ) where

import Data.ByteString as BS
import Foreign
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import System.Posix.FilePath

import System.UDev.Context
import System.UDev.Device
import System.UDev.List
import System.UDev.Types


foreign import ccall unsafe "udev_enumerate_new"
  c_new :: UDev -> IO Enumerate

-- | Create an enumeration context to scan /sys.
newEnumerate :: UDev -> IO Enumerate
newEnumerate = c_new
{-# INLINE newEnumerate #-}

{-----------------------------------------------------------------------
--  Match
-----------------------------------------------------------------------}

foreign import ccall unsafe "udev_enumerate_add_match_subsystem"
  c_addMatchSubsystem :: Enumerate -> CString -> IO CInt

-- | Kernel subsystem string.
type Subsystem = ByteString

-- | Match only devices belonging to a certain kernel subsystem.
addMatchSubsystem :: Enumerate -- ^ context
                  -> Subsystem -- ^ filter for a subsystem of the
                               -- device to include in the list
                  -> IO ()     -- ^ can throw exception
addMatchSubsystem enumerate subsystem =
  throwErrnoIfMinus1_ "addMatchSubsystem" $
    useAsCString subsystem $
      c_addMatchSubsystem enumerate

foreign import ccall unsafe "udev_enumerate_add_nomatch_subsystem"
  c_addNoMatchSubsystem :: Enumerate -> CString -> IO CInt

-- | Match only devices not belonging to a certain kernel subsystem.
addNoMatchSubsystem :: Enumerate -- ^ context
                    -> Subsystem -- ^ filter for a subsystem of the
                                 -- device to exclude from the list
                    -> IO ()     -- ^ can throw exception
addNoMatchSubsystem enumerate subsystem =
  throwErrnoIfMinus1_ "addNoMatchSubsystem" $
    useAsCString subsystem $
      c_addNoMatchSubsystem enumerate

-- | \/sys attribute string.
type SysAttr  = ByteString

-- | Attribute specific \/sys value string. Can be an int or
-- identifier depending on attribute.
type SysValue = ByteString

foreign import ccall unsafe "udev_enumerate_add_match_sysattr"
  c_addMatchSysattr :: Enumerate -> CString -> CString -> IO CInt

-- | Match only devices with a certain \/sys device attribute.
addMatchSysattr :: Enumerate      -- ^ context
                -> SysAttr        -- ^ filter for a sys attribute at
                                  -- the device to include in the list
                -> Maybe SysValue -- ^ optional value of the sys attribute
                -> IO ()          -- ^ can throw exception
addMatchSysattr enumerate sysattr mvalue =
  throwErrnoIf_ (< 0) "addMatchSysattr" $
    useAsCString sysattr $ \ c_sysattr ->
      case mvalue of
        Nothing    -> c_addMatchSysattr enumerate c_sysattr nullPtr
        Just value ->
          useAsCString value $ \ c_value ->
            c_addMatchSysattr enumerate c_sysattr c_value

foreign import ccall unsafe  "udev_enumerate_add_nomatch_sysattr"
  c_addNoMatchSysattr :: Enumerate -> CString -> CString -> IO CInt

-- | Match only devices not having a certain /sys device attribute.
addNoMatchSysattr :: Enumerate  -- ^ context
                  -> ByteString -- ^ filter for a sys attribute at the
                                -- device to exclude from the list
                  -> Maybe ByteString -- ^ optional value of the sys
                                      -- attribute
                  -> IO ()
addNoMatchSysattr enumerate sysattr mvalue =
  throwErrnoIf_ (< 0) "addNoMatchSysattr" $
    useAsCString sysattr $ \ c_sysattr ->
      case mvalue of
        Nothing    -> c_addNoMatchSysattr enumerate c_sysattr nullPtr
        Just value ->
          useAsCString value $ \ c_value ->
            c_addNoMatchSysattr enumerate c_sysattr c_value

foreign import ccall unsafe "udev_enumerate_add_match_property"
  c_addMatchProperty :: Enumerate -> CString -> CString -> IO CInt

-- | Match only devices with a certain property.
addMatchProperty :: Enumerate  -- ^ context
                 -> ByteString -- ^ filter for a property of the
                               -- device to include in the list
                 -> ByteString -- ^ value of the property
                 -> IO ()
addMatchProperty enumerate prop value =
  throwErrnoIf_ (< 0) "addMatchProperty" $
    useAsCString prop $ \ c_prop ->
      useAsCString value $ \ c_value ->
        c_addMatchProperty enumerate c_prop c_value

foreign import ccall unsafe "udev_enumerate_add_match_tag"
  c_addMatchTag :: Enumerate -> CString -> IO CInt


-- | Match only devices with a certain tag.
addMatchTag :: Enumerate  -- ^ context
            -> ByteString -- ^ filter for a tag of the device to
                          -- include in the list
            -> IO ()
addMatchTag enumerate tag =
  throwErrnoIf_ (< 0) "addMatchTag" $
    useAsCString tag $ \ c_tag ->
      c_addMatchTag enumerate c_tag

foreign import ccall unsafe "udev_enumerate_add_match_parent"
  c_addMatchParent :: Enumerate -> Device -> IO CInt

-- | Return the devices on the subtree of one given device. The parent
-- itself is included in the list.
--
-- A reference for the device is held until the udev_enumerate context
-- is cleaned up.
--
addMatchParent :: Enumerate -- ^ context
               -> Device    -- ^ parent device where to start searching
               -> IO ()     -- ^ can throw exception
addMatchParent enumerate dev =
  throwErrnoIf_ (< 0) "addMatchParent" $
    c_addMatchParent enumerate dev

foreign import ccall unsafe "udev_enumerate_add_match_is_initialized"
  c_addMatchIsInitialized :: Enumerate -> IO CInt

-- | Match only devices which udev has set up already.
addMatchIsInitialized :: Enumerate -> IO ()
addMatchIsInitialized enumerate =
  throwErrnoIfMinus1_ "addMatchIsInitialized" $
    c_addMatchIsInitialized enumerate

foreign import ccall unsafe "udev_enumerate_add_match_sysname"
  c_addMatchSysname :: Enumerate -> CString -> IO CInt

-- | Match only devices with a given \/sys device name.
addMatchSysname :: Enumerate  -- ^ context
                -> ByteString -- ^ filter for the name of the device
                              -- to include in the list
                -> IO ()      -- ^ can throw exception
addMatchSysname enumerate sysName =
  throwErrnoIfMinus1_ "addMatchSysname" $
    useAsCString sysName $
      c_addMatchSysname enumerate

foreign import ccall unsafe "udev_enumerate_add_syspath"
  c_addSyspath :: Enumerate -> CString -> IO CInt

-- | Add a device to the list of devices, to retrieve it back sorted
-- in dependency order.
--
addSyspath :: Enumerate   -- ^ context
           -> RawFilePath -- ^ path of a device
           -> IO ()       -- ^ can throw exception
addSyspath enumerate syspath =
  throwErrnoIf_ (< 0) "addSyspath" $
    useAsCString syspath $ \ c_syspath ->
      c_addSyspath enumerate c_syspath

{-----------------------------------------------------------------------
--  Scan
-----------------------------------------------------------------------}

foreign import ccall unsafe "udev_enumerate_scan_devices"
  c_scanDevices :: Enumerate -> IO CInt

-- | Scan \/sys for all devices which match the given filters.
scanDevices :: Enumerate -> IO ()
scanDevices = throwErrnoIfMinus1_ "scanDevices" . c_scanDevices

foreign import ccall unsafe "udev_enumerate_scan_subsystems"
  c_scanSubsystems :: Enumerate -> IO CInt

-- | Scan \/sys for all devices which match the given filters.
scanSubsystems :: Enumerate -> IO ()
scanSubsystems = throwErrnoIfMinus1_ "scanSubsystems" . c_scanSubsystems

{-----------------------------------------------------------------------
--  Query
-----------------------------------------------------------------------}

foreign import ccall unsafe "udev_enumerate_get_list_entry"
  c_getListEntry :: Enumerate -> IO List

-- | Get the first entry of the sorted list of device paths.
getListEntry :: Enumerate -> IO (Maybe List)
getListEntry enumerate = do
  xs <- c_getListEntry enumerate
  return $ if xs == nil then Nothing else Just xs
{-# INLINE getListEntry #-}
