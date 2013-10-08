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
       , addMatchSubsystem
       , addNoMatchSubsystem
       , addMatchSysattr
         -- TODO
       , addMatchIsInitialized
       , addMatchSysname
         -- TODO

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

import System.UDev.Context
import System.UDev.List
import System.UDev.Types

-- | Opaque object representing one device lookup/sort context.
newtype Enumerate = Enumerate (Ptr Enumerate)

foreign import ccall unsafe "udev_enumerate_ref"
  c_ref :: Enumerate -> IO Enumerate

foreign import ccall unsafe "udev_enumerate_unref"
  c_unref :: Enumerate -> IO Enumerate

instance Ref Enumerate where
  ref   = c_ref
  unref = c_unref

foreign import ccall unsafe "udev_enumerate_get_udev"
  c_getUDev :: Enumerate -> UDev

instance UDevChild Enumerate where
  getUDev = c_getUDev

foreign import ccall unsafe "udev_enumerate_new"
  c_new :: UDev -> IO Enumerate

-- | Create an enumeration context to scan /sys.
newEnumerate :: UDev -> IO Enumerate
newEnumerate = c_new
{-# INLINE newEnumerate #-}

foreign import ccall unsafe "udev_enumerate_add_match_subsystem"
  c_addMatchSubsystem :: Enumerate -> CString -> IO CInt

type Subsystem = ByteString

-- | Match only devices belonging to a certain kernel subsystem.
addMatchSubsystem :: Enumerate -> Subsystem -> IO ()
addMatchSubsystem enumerate subsystem = do
  throwErrnoIfMinus1_ "addMatchSubsystem" $ do
    useAsCString subsystem $
      c_addMatchSubsystem enumerate

foreign import ccall unsafe "udev_enumerate_add_nomatch_subsystem"
  c_addNoMatchSubsystem :: Enumerate -> CString -> IO CInt

-- | Match only devices not belonging to a certain kernel subsystem.
addNoMatchSubsystem :: Enumerate -> Subsystem -> IO ()
addNoMatchSubsystem enumerate subsystem = do
  throwErrnoIfMinus1_ "addNoMatchSubsystem" $ do
    useAsCString subsystem $
      c_addNoMatchSubsystem enumerate

type SysAttr  = ByteString
type SysValue = ByteString

foreign import ccall unsafe "udev_enumerate_add_match_sysattr"
  c_addMatchSysattr :: Enumerate -> CString -> CString -> IO CInt

-- | Match only devices with a certain \/sys device attribute.
addMatchSysattr :: Enumerate -> SysAttr -> SysValue -> IO ()
addMatchSysattr enumerate sysAttr sysVal = do
  throwErrnoIfMinus1_ "addMatchSysattr" $ do
    useAsCString sysAttr $ \ cSysAttr ->
      useAsCString sysVal $ \ cSysVal ->
        c_addMatchSysattr enumerate cSysAttr cSysVal

foreign import ccall unsafe "udev_enumerate_add_match_is_initialized"
  c_addMatchIsInitialized :: Enumerate -> IO CInt

-- | Match only devices which udev has set up already.
addMatchIsInitialized :: Enumerate -> IO ()
addMatchIsInitialized enumerate = do
  throwErrnoIfMinus1_ "addMatchIsInitialized" $ do
    c_addMatchIsInitialized enumerate

foreign import ccall unsafe "udev_enumerate_add_match_sysname"
  c_addMatchSysname :: Enumerate -> CString -> IO CInt

-- | Match only devices with a given \/sys device name.
addMatchSysname :: Enumerate -> ByteString -> IO ()
addMatchSysname enumerate sysName = do
  throwErrnoIfMinus1_ "addMatchSysname" $ do
    useAsCString sysName $
      c_addMatchSysname enumerate

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

foreign import ccall unsafe "udev_enumerate_get_list_entry"
  c_getListEntry :: Enumerate -> IO List

-- | Get the first entry of the sorted list of device paths.
getListEntry :: Enumerate -> IO List
getListEntry = c_getListEntry
{-# INLINE getListEntry #-}
