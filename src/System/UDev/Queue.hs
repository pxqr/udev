-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   The udev daemon processes events asynchronously. All events which
--   do not have interdependencies run in parallel. This exports the
--   current state of the event processing queue, and the current
--   event sequence numbers from the kernel and the udev daemon.
--
module System.UDev.Queue
       ( Queue
       , Seqnum

       , newQueue
       , isActive
       , isEmpty
       , isFinished

       , getPending
       , sequenceIsFinished
       , getKernelSeqnum
       , getUDevSeqnum
       ) where

import Control.Applicative
import Foreign.C

import System.UDev.Context
import System.UDev.List
import System.UDev.Types


foreign import ccall unsafe "udev_queue_new"
  c_newQueue :: UDev -> IO Queue

-- | Create a new queue.
newQueue :: UDev -> IO Queue
newQueue = c_newQueue
{-# INLINE newQueue #-}

foreign import ccall unsafe "udev_queue_get_udev_is_active"
  c_isActive :: Queue -> IO CInt

-- | Check if udev is active on the system.
isActive :: Queue -> IO Bool
isActive queue = (0 <) <$> c_isActive queue
{-# INLINE isActive #-}

foreign import ccall unsafe "udev_queue_get_queue_is_empty"
  c_isEmpty :: Queue -> IO CInt

-- | Check if udev is currently processing any events.
isEmpty :: Queue -> IO Bool
isEmpty queue = (0 <) <$> c_isEmpty queue
{-# INLINE isEmpty #-}

foreign import ccall unsafe "udev_queue_get_seqnum_is_finished"
  c_isFinished :: Queue -> CULLong -> IO CInt

-- | Sequence number of event.
type Seqnum = Int

-- | Check if udev is currently processing a given sequence number.
isFinished :: Queue   -- ^ udev queue context
           -> Seqnum  -- ^ sequence number
           -> IO Bool -- ^ if the given sequence number is currently
                      -- active.
isFinished queue seqnr = (0 <) <$> c_isFinished queue (fromIntegral seqnr)
{-# INLINE isFinished #-}

foreign import ccall unsafe "udev_queue_get_seqnum_sequence_is_finished"
  c_sequenceIsFinished :: Queue -> CULLong -> CULLong -> IO CInt

-- | Check if udev is currently processing any events in a given
-- sequence number range.
--
sequenceIsFinished :: Queue   -- ^ udev queue context
                   -> Seqnum  -- ^ first event sequence number
                   -> Seqnum  -- ^ last event sequence number
                   -> IO Bool -- ^ if any of the sequence numbers in
                              -- the given range is currently active
sequenceIsFinished queue start end =
  (> 0) <$> c_sequenceIsFinished queue (fromIntegral start)
                                       (fromIntegral end)
{-# INLINE sequenceIsFinished #-}

foreign import ccall unsafe "udev_queue_get_queued_list_entry"
  c_getPending :: Queue -> IO List

-- | Get the first entry of the list of queued events.
getPending :: Queue -> IO List
getPending = c_getPending
{-# INLINE getPending #-}

foreign import ccall unsafe "udev_queue_get_kernel_seqnum"
  c_getKernelSeqnum :: Queue -> IO CULLong

-- | Get the current kernel event sequence number.
getKernelSeqnum :: Queue -> IO Seqnum
getKernelSeqnum queue = fromIntegral <$> c_getKernelSeqnum queue
{-# INLINE getKernelSeqnum #-}

foreign import ccall unsafe "udev_queue_get_udev_seqnum"
  c_getUDevSeqnum :: Queue -> IO CULLong

-- | Get the last known udev event sequence number.
getUDevSeqnum :: Queue -> IO Seqnum
getUDevSeqnum queue = fromIntegral <$> c_getUDevSeqnum queue
{-# INLINE getUDevSeqnum #-}
