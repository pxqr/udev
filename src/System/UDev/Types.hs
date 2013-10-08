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
       ) where

class Ref a where
  ref   :: a -> IO a
  unref :: a -> IO a
