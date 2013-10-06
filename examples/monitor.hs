{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import Data.ByteString.Char8 as BC

import System.UDev.Context
import System.UDev.Device
import System.UDev.Monitor
import System.Posix.IO.Select
import System.Posix.IO.Select.Types


dumpDeviceInfo :: Device -> IO ()
dumpDeviceInfo dev = do
  print $ getSubsystem dev
  print $ getDevtype   dev
  print $ getSyspath   dev
  print $ getSysname   dev
  print $ getSysnum    dev
  print $ getAction    dev

main :: IO ()
main = do
  withUDev $ \ udev -> do
    monitor <- newFromNetlink udev udevId
    enableReceiving monitor
    fd <- getFd monitor
    forever $ do
      res <- select' [fd] [] [] Never
      case res of
        Just ([_], [], []) -> dumpDeviceInfo =<< receiveDevice monitor
        Nothing -> return ()
    return ()