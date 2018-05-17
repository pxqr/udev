{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forever)
import Control.Concurrent (threadWaitRead)

import System.UDev.Context
import System.UDev.Device
import System.UDev.Monitor

dumpDeviceInfo :: Device -> IO ()
dumpDeviceInfo dev = do
  print $ getSubsystem dev
  print $ getDevtype   dev
  print $ getSyspath   dev
  print $ getSysname   dev
  print $ getSysnum    dev
  print $ getDevnode    dev
  print $ getAction    dev
  putStrLn $ "device number: " ++ show (getDevnum dev)

main :: IO ()
main = withUDev $ \ udev -> do
  setLogPriority udev LogDebug
  setLogger udev defaultLogger
  monitor <- newFromNetlink udev UDevId
  enableReceiving monitor
  fd <- getFd monitor
  forever $ do
    threadWaitRead fd
    dev <- receiveDevice monitor
    dumpDeviceInfo dev
