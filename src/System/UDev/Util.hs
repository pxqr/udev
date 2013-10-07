-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Utilities useful when dealing with devices and device node names.
--
module System.UDev.Util
       ( encodeString
       ) where

import Foreign.C


foreign import ccall unsafe "udev_util_encode_string"
  c_encodeString :: CString -> CString -> CSize -> IO ()

-- | Encode all potentially unsafe characters of a string to the
-- corresponding 2 char hex value prefixed by '\x'.
--
encodeString :: CString -> CString -> CSize -> IO ()
encodeString = c_encodeString