{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module MsdnGrabber.WebPage where

import Data.Aeson
import Data.Tree

import Control.Applicative

import System.FilePath.Posix as Posix

data WebPageLink = WebPageLink { wpLink :: String
                               , wpName :: String
                               } deriving (Eq, Show)

wpFilename :: WebPageLink -> FilePath
wpFilename = Posix.takeFileName . wpLink

instance Ord WebPageLink where
    compare x y = compare (wpName x) (wpName y)

newPage :: String -> String -> Forest WebPageLink -> Tree WebPageLink
newPage l n p = Node (WebPageLink l n) p

pageLink :: Tree WebPageLink -> String
pageLink = wpLink . rootLabel

pageName :: Tree WebPageLink -> String
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
