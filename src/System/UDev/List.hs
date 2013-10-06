module System.UDev.List
       ( List (..)

       , getNext
       , getByName

       , getName
       , getValue
       ) where

import Control.Monad
import Data.ByteString
import Foreign
import Foreign.C.String
import Foreign.C.Types


newtype List = List (Ptr List)
               deriving Eq

nil :: List
nil = List nullPtr

foreign import ccall unsafe "udev_list_entry_get_next"
  c_getNext :: List -> IO List

foreign import ccall unsafe "udev_list_entry_get_by_name"
  c_getByName :: List -> IO List

foreign import ccall unsafe "udev_list_entry_get_name"
  c_getName :: List -> IO CString

foreign import ccall unsafe "udev_list_entry_get_value"
  c_getValue :: List -> IO CString

getNext :: List -> IO (Maybe List)
getNext xxs = do
  xs <- c_getNext xxs
  return $ if xs == nil then Nothing else Just xs

getByName :: List -> IO (Maybe List)
getByName xs = do
  ys <- c_getByName xs
  return $ if ys == nil then Nothing else Just ys

-- TODO avoid copying?
getName :: List -> IO ByteString
getName = c_getName >=> packCString

getValue :: List -> IO ByteString
getValue = c_getValue >=> packCString