{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import Data.ByteString.Char8 as BC

import System.UDev.Context
import System.UDev.Device
import System.UDev.Monitor
import System.Posix.IO.Select
import System.Posix.IO.Select.Types


deviceInfo :: Device -> IO ()
deviceInfo dev = print $ getAction dev

main :: IO ()
main = do
  withUDev $ \ udev -> do
    monitor <- newFromNetlink udev udevId
    enableReceiving monitor
    fd <- getFd monitor
    forever $ do
      res <- select' [fd] [] [] Never
      case res of
        Just ([_], [], []) -> do
          dev <- receiveDevice monitor
          deviceInfo dev
        Nothing -> return ()
    return ()