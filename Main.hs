{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import Data.Tree

import Control.Monad

import GHC.IO.Encoding (setLocaleEncoding, setFileSystemEncoding, setForeignEncoding, utf8)

import MsdnGrabber.Topic
import MsdnGrabber.Grabber
import MsdnGrabber.Parser
import MsdnGrabber.Emit.Json()

main :: IO ()
main = do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    setForeignEncoding utf8
    pages <- downloadPages "/en-us/library/bb162138.aspx"
    pages <- liftM (fromJust . decode) $ BL.readFile "raw\\index.json"
    topics <- parsePages pages
    forM_ (flatten topics) $ \t -> do
        let filename = topicFilename t
        BL.writeFile ("data\\" ++ filename) $ encodePretty t
