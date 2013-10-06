{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString as BS
import Data.ByteString.Char8 as BC

import System.UDev.Context
import System.UDev.Device
import System.UDev.Enumerate
import System.UDev.List


main :: IO ()
main = do
  withUDev $ \ udev -> do
    e <- newEnumerate udev
    addMatchSubsystem e "hidraw"
    scanDevices e
    ls <- getListEntry e
    path  <- getName ls
    print path
    dev <- newFromSysPath udev path
    print =<< getDevNode dev
    mdev <- getParentWithSubsystemDevtype dev "usb" "usb_device"
    case mdev of
      Nothing   -> BC.putStrLn "unable to find parent usb device"
      Just pdev -> do
        BC.putStrLn $ BS.concat
          [ "VID/PID: ", getSysattrValue pdev "idVendor" , " "
                       , getSysattrValue pdev "idProduct", "\n"
          , getSysattrValue pdev "manufacturer", "\n"
          , getSysattrValue pdev "product"     , "\n"
--          , "serial: ",  getSysattrValue pdev "serial"
          ]
    return ()
