{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module MsdnGrabber.WebPage where

import Data.Aeson
import Data.Tree

import Control.Applicative
import Control.Monad

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor
import Text.XML.Scraping
import Text.XML.Selector.TH

import System.FilePath.Posix as Posix

data WebPageLink = WebPageLink { wpLink :: String
                               , wpName :: String
                               } deriving (Eq, Show)
wpFilename = Posix.takeFileName . wpLink

instance Ord WebPageLink where
    compare x y = compare (wpName x) (wpName y)

type WebPage = Tree WebPageLink

newPage l n p = Node (WebPageLink l n) p
pageLink = wpLink . rootLabel
pageName = wpName . rootLabel

instance ToJSON (Tree WebPageLink) where
    toJSON tree = object [ "link" .= pageLink tree
                         , "name" .= pageName tree
                         , "pages" .= subForest tree
                         ]

instance FromJSON (Tree WebPageLink) where
    parseJSON (Object x) = newPage <$> x .: "link"
                                   <*> x .: "name"
                                   <*> x .: "pages"
