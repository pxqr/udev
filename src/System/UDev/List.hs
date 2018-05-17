-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Libudev list operations.
--
module System.UDev.List
       ( List

       , getNext
       , getByName

       , getName
       , getValue
       ) where

import Control.Monad
import Data.ByteString
import Foreign.C.String

import System.UDev.Types

foreign import ccall unsafe "udev_list_entry_get_next"
  c_getNext :: List -> IO List

foreign import ccall unsafe "udev_list_entry_get_by_name"
  c_getByName :: List -> IO List

foreign import ccall unsafe "udev_list_entry_get_name"
  c_getName :: List -> IO CString

foreign import ccall unsafe "udev_list_entry_get_value"
  c_getValue :: List -> IO CString

-- | Get the next entry from the list.
getNext :: List -> IO (Maybe List)
getNext xxs = do
  xs <- c_getNext xxs
  return $ if xs == nil then Nothing else Just xs

-- | Lookup an entry in the list with a certain name.
getByName :: List -> IO (Maybe List)
getByName xs = do
  ys <- c_getByName xs
  return $ if ys == nil then Nothing else Just ys

-- TODO avoid copying?
-- | Get the name of a list entry.
getName :: List -> IO ByteString
getName = c_getName >=> packCString

-- | Get the value of list entry.
getValue :: List -> IO ByteString
getValue = c_getValue >=> packCString
