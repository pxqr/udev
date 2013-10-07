module System.UDev.Queue
       ( Queue

       , newQueue
       , isActive
       , isEmpty
       , isFinished

       , getPending
       , getKernelSeqnum
       , getUDevSeqnum
       ) where

import Control.Applicative
import Foreign
import Foreign.C

import System.UDev.Context
import System.UDev.List
import System.UDev.Types


-- | Opaque object representing the current event queue in the udev
-- daemon.
newtype Queue = Queue { getQueue :: Ptr Queue }

foreign import ccall unsafe "udev_queue_ref"
  c_ref :: Queue -> IO Queue

foreign import ccall unsafe "udev_queue_unref"
  c_unref :: Queue -> IO Queue

instance Ref Queue where
  ref   = c_ref
  unref = c_unref

foreign import ccall unsafe "udev_queue_new"
  c_newQueue :: UDev -> IO Queue

newQueue :: UDev -> IO Queue
newQueue = c_newQueue

foreign import ccall unsafe "udev_queue_get_udev_is_active"
  c_isActive :: Queue -> IO CInt

isActive :: Queue -> IO Bool
isActive queue = (0 <) <$> c_isActive queue

foreign import ccall unsafe "udev_queue_get_queue_is_empty"
  c_isEmpty :: Queue -> IO CInt

isEmpty :: Queue -> IO Bool
isEmpty queue = (0 <) <$> c_isEmpty queue

foreign import ccall unsafe "udev_queue_get_seqnum_is_finished"
  c_isFinished :: Queue -> CULLong -> IO CInt

type Seqnum = Int

-- | Check if udev is currently processing a given sequence number.
isFinished :: Queue -> Seqnum -> IO Bool
isFinished queue seqnr = (0 <) <$> c_isFinished queue (fromIntegral seqnr)

foreign import ccall unsafe "udev_queue_get_queued_list_entry"
  c_getPending :: Queue -> IO List

-- | Get the first entry of the list of queued events.
getPending :: Queue -> IO List
getPending = c_getPending

foreign import ccall unsafe "udev_queue_get_kernel_seqnum"
  c_getKernelSeqnum :: Queue -> IO CULLong

-- | Get the current kernel event sequence number.
getKernelSeqnum :: Queue -> IO Seqnum
getKernelSeqnum queue = fromIntegral <$> c_getKernelSeqnum queue

foreign import ccall unsafe "udev_queue_get_udev_seqnum"
  c_getUDevSeqnum :: Queue -> IO CULLong

-- | Get the last known udev event sequence number.
getUDevSeqnum :: Queue -> IO Seqnum
getUDevSeqnum queue = fromIntegral <$> c_getUDevSeqnum queue