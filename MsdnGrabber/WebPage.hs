{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module MsdnGrabber.WebPage where

import Data.Aeson
import Data.Text
import Data.Tree

import Control.Applicative

import MsdnGrabber.Utilities

data WebPageLink = WebPageLink { wpLink :: Text
                               , wpName :: Text
                               } deriving (Eq, Show)

wpFilename :: WebPageLink -> Text
wpFilename = takeFileName . wpLink

instance Ord WebPageLink where
    compare x y = compare (wpName x) (wpName y)

newPage :: Text -> Text -> Forest WebPageLink -> Tree WebPageLink
newPage l n = Node (WebPageLink l n)

pageLink :: Tree WebPageLink -> Text
pageLink = wpLink . rootLabel

pageName :: Tree WebPageLink -> Text
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
