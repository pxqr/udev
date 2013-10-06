module System.UDev.Types
       ( Ref (..)
       ) where

class Ref a where
  ref   :: a -> IO a
  unref :: a -> IO a
