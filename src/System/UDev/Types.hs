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
       , List   (..), nil
       ) where

import Foreign

{-----------------------------------------------------------------------
--  Classes
-----------------------------------------------------------------------}

class Ref a where
  ref   :: a -> IO a
  unref :: a -> IO a

class UDevChild a where
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

