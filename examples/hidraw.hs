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
    Just ls <- getListEntry e
    path  <- getName ls
    print path
    dev <- newFromSysPath udev path
    print $ getDevnode dev
    mdev <- getParentWithSubsystemDevtype dev "usb" "usb_device"
    case mdev of
      Nothing   -> BC.putStrLn "unable to find parent usb device"
      Just pdev -> do
        vid <- getSysattrValue pdev "idVendor"
        pid <- getSysattrValue pdev "idProduct"
        manifacturer <- getSysattrValue pdev "manufacturer"
        productName  <- getSysattrValue pdev "product"

        BC.putStrLn $ BS.concat
          [ "VID/PID: ", vid, " "
                       , pid, "\n"
          , manifacturer, "\n"
          , productName , "\n"
--          , "serial: ",  getSysattrValue pdev "serial"
          ]
    return ()
