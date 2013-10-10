-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   Retrieve properties from the hardware database.
--
module System.UDev.HWDB
       ( HWDB
       , newHWDB
       , getPropertiesList
       ) where

import Data.ByteString as BS
import Foreign.C

import System.UDev.Context
import System.UDev.List
import System.UDev.Types


foreign import ccall unsafe "udev_hwdb_new"
  c_new :: UDev -> IO HWDB

-- | Create a hardware database context to query properties for
-- devices.
newHWDB :: UDev -> IO HWDB
newHWDB = c_new

foreign import ccall unsafe "udev_hwdb_get_properties_list_entry"
  c_getPropertiesList :: HWDB -> CString -> CUInt -> IO List

-- | Lookup a matching device in the hardware database. The lookup key
-- is a modalias string, whose formats are defined for the Linux
-- kernel modules. Examples are: pci:v00008086d00001C2D*,
-- usb:v04F2pB221*. The first entry of a list of retrieved properties
-- is returned.
--
getPropertiesList :: HWDB -> ByteString -> IO List
getPropertiesList hwdb modalias =
  useAsCString modalias $ \ c_modalias ->
    c_getPropertiesList hwdb c_modalias 0
