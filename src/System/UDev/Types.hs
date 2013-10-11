-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Internal definitions; do not export this module.
--
module System.UDev.Types
       ( Ref (..)
       , UDevChild (..)

       , UDev   (..)
       , Device (..)
       , Enumerate (..)
       , HWDB   (..)
       , List   (..), nil
       , Queue  (..)
       ) where

import Foreign

{-----------------------------------------------------------------------
--  Classes
-----------------------------------------------------------------------}

class Ref a where
  ref   :: a -> IO a
  unref :: a -> IO a

-- | Family of udev resources.
class UDevChild a where
  -- | Get the context a resource belong to.
  getUDev :: a -> UDev

{-----------------------------------------------------------------------
--  UDev
-----------------------------------------------------------------------}

-- | Opaque object representing the library context.
newtype UDev = UDev (Ptr UDev)


instance UDevChild UDev where
  getUDev = id

foreign import ccall unsafe "udev_ref"
  c_ref :: UDev -> IO UDev

foreign import ccall unsafe "udev_unref"
  c_unref :: UDev -> IO UDev

instance Ref UDev where
  ref   = c_ref
  unref = c_unref

{-----------------------------------------------------------------------
--  Device
-----------------------------------------------------------------------}

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

instance UDevChild Device where
  getUDev = c_getUDev

{-----------------------------------------------------------------------
--  Enumerate
-----------------------------------------------------------------------}

-- | Opaque object representing one device lookup/sort context.
newtype Enumerate = Enumerate (Ptr Enumerate)

foreign import ccall unsafe "udev_enumerate_ref"
  c_EnumerateRef :: Enumerate -> IO Enumerate

foreign import ccall unsafe "udev_enumerate_unref"
  c_EnumerateUnref :: Enumerate -> IO Enumerate

instance Ref Enumerate where
  ref   = c_EnumerateRef
  unref = c_EnumerateUnref

foreign import ccall unsafe "udev_enumerate_get_udev"
  c_EnumerateGetUDev :: Enumerate -> UDev

instance UDevChild Enumerate where
  getUDev = c_EnumerateGetUDev

{-----------------------------------------------------------------------
--  HWDB
-----------------------------------------------------------------------}

-- | Opaque object representing the hardware database.
newtype HWDB = HWDB (Ptr HWDB)

foreign import ccall unsafe "udev_hwdb_ref"
  c_HWDBRef :: HWDB -> IO HWDB

foreign import ccall unsafe "udev_hwdb_unref"
  c_HWDBUnref :: HWDB -> IO HWDB

instance Ref HWDB where
  ref   = c_HWDBRef
  unref = c_HWDBUnref

{-----------------------------------------------------------------------
--  List
-----------------------------------------------------------------------}

-- TODO newtype List name value

-- | Opaque object representing one entry in a list. An entry contains
-- contains a name, and optionally a value.
--
newtype List = List (Ptr List)
               deriving Eq

nil :: List
nil = List nullPtr

{-----------------------------------------------------------------------
--  Queue
-----------------------------------------------------------------------}

-- | Opaque object representing the current event queue in the udev
-- daemon.
newtype Queue = Queue { getQueue :: Ptr Queue }

foreign import ccall unsafe "udev_queue_ref"
  c_queueRef :: Queue -> IO Queue

foreign import ccall unsafe "udev_queue_unref"
  c_queueUnref :: Queue -> IO Queue

instance Ref Queue where
  ref   = c_queueRef
  unref = c_queueUnref
