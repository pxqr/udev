module System.UDev.HWDB
       ( HWDB
       , newHWDB
       , getPropertiesList
       ) where

import Data.ByteString as BS
import Foreign
import Foreign.C

import System.UDev.Context
import System.UDev.List


-- | Opaque object representing the hardware database.
newtype HWDB = HWDB (Ptr HWDB)

foreign import ccall unsafe "udev_hwdb_ref"
  c_ref :: HWDB -> IO HWDB

foreign import ccall unsafe "udev_hwdb_unref"
  c_unref :: HWDB -> IO HWDB

foreign import ccall unsafe "udev_hwdb_new"
  c_new :: UDev -> IO HWDB

-- | Create a hardware database context to query properties for
-- devices.
newHWDB :: UDev -> IO HWDB
newHWDB = c_new

foreign import ccall unsafe "udev_hwdb_get_properties_list_entry"
  c_getPropertiesList :: HWDB -> CString -> CUInt -> IO List

getPropertiesList :: HWDB -> ByteString -> IO List
getPropertiesList hwdb modalias =
  useAsCString modalias $ \ c_modalias ->
    c_getPropertiesList hwdb c_modalias 0
