module System.UDev ( ) where

import Data.Monoid


type Subsystem = ()
type Devtype   = ()
type Tag       = ()

data Filter = Filter (Maybe (Subsystem, Devtype)) (Maybe Tag)

instance Monoid Filter where
  mempty  = Filter Nothing Nothing
  Filter sda ta `mappend` Filter sdb tb
    = Filter (sda `mappend` sdb) (ta `mappend` tb)