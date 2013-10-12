{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude as P

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
  print $ getDevnode    dev
  print $ getAction    dev
  P.putStrLn $ "device number: " ++ show (getDevnum dev)

main :: IO ()
main = do
  withUDev $ \ udev -> do
    setLogPriority udev LogDebug
    setLogger udev defaultLogger
    monitor <- newFromNetlink udev UDevId
    enableReceiving monitor
    fd <- getFd monitor
    forever $ do
      res <- select' [fd] [] [] Never
      case res of
        Just ([_], [], []) -> do
          dev <- receiveDevice monitor
          dumpDeviceInfo dev
        Nothing -> return ()
    return ()