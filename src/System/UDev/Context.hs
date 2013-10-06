-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
{-# LANGUAGE ForeignFunctionInterface #-}
module System.UDev.Context
       ( UDev (..)
       , UDevChild (..)

       , withUDev
       ) where

import Control.Exception
import Data.ByteString as BS
import Foreign
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types


newtype UDev = UDev (Ptr UDev)

class UDevChild a where
  getUDev :: a -> UDev

instance UDevChild UDev where
  getUDev = id

foreign import ccall unsafe "udev_new"
  udev_new :: IO UDev

foreign import ccall unsafe "udev_ref"
  udev_ref :: UDev -> IO UDev

foreign import ccall unsafe "udev_unref"
  udev_unref :: UDev -> IO UDev

newUDev :: IO UDev
newUDev = undefined

withUDev :: (UDev -> IO a) -> IO a
withUDev = bracket udev_new udev_unref
