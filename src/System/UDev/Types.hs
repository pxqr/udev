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

       , UDev (..)
       ) where

import Foreign


class Ref a where
  ref   :: a -> IO a
  unref :: a -> IO a

-- | Opaque object representing the library context.
newtype UDev = UDev (Ptr UDev)

class UDevChild a where
  getUDev :: a -> UDev

instance UDevChild UDev where
  getUDev = id

foreign import ccall unsafe "udev_ref"
  c_ref :: UDev -> IO UDev

foreign import ccall unsafe "udev_unref"
  c_unref :: UDev -> IO UDev

instance Ref UDev where
  ref   = c_ref
  unref = c_unref
