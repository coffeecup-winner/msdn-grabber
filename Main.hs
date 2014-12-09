{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe

import Control.Monad

import GHC.IO.Encoding (setLocaleEncoding, setFileSystemEncoding, setForeignEncoding, utf8)

import MsdnGrabber.Grabber
import MsdnGrabber.Converter

main :: IO ()
main = do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    setForeignEncoding utf8
    pages <- downloadPages "/en-us/library/bb162138.aspx"
    pages <- liftM (fromJust . decode) $ BL.readFile "raw\\index.json"
    processPages pages
