{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MsdnGrabber.Grabber where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Text.Lazy.IO as LI
import Data.Tree

import Control.Applicative
import Control.Concurrent.ParallelIO.Local

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor
import Text.XML.Scraping
import Text.XML.Selector.TH

import Network (withSocketsDo)
import Network.HTTP.Conduit

import System.Directory (createDirectoryIfMissing)

import MsdnGrabber.HtmlSelectors
import MsdnGrabber.Utilities
import MsdnGrabber.WebPage

withCursor :: T.Text -> (Cursor -> IO a) -> IO a
withCursor url f = withSocketsDo $ do
    root <- (fromDocument . parseLBS) <$> simpleHttp (T.unpack url)
    f root

findSubpages :: Cursor -> [Cursor]
findSubpages c = if not . null . queryT [jq| #tocnav > div.toclevel2.current |] $ c
    then []
    else (queryT [jq| #tocnav > div.toclevel2 |] &/ anyElement) c

generateRequest :: Cursor -> WebPageLink
generateRequest = WebPageLink <$> href <*> text

baseUrl :: T.Text
baseUrl = "http://msdn.microsoft.com"

downloadPages :: Int -> String -> IO (Tree WebPageLink)
downloadPages threadsCount url = do
    let request = WebPageLink (T.pack url) ""
    createDirectoryIfMissing False "raw"
    pages <- withPool threadsCount $ \pool -> grab pool request
    save pages
    return pages

grab :: Pool -> WebPageLink -> IO (Tree WebPageLink)
grab pool req = withCursor (baseUrl <> wpLink req) $ \root -> do
    TI.putStrLn $ wpLink req <> " " <> wpName req
    LI.writeFile (T.unpack $ "raw\\" <> wpFilename req) (innerHtml root)
    !pages <- go $ root $// findSubpages &| generateRequest
    return $ newPage (wpLink req) (wpName req) pages
    where
        go :: [WebPageLink] -> IO [Tree WebPageLink]
        go rs = parallel pool $ fmap (grab pool) (distinct rs)

save :: Tree WebPageLink -> IO ()
save pages = BL.writeFile "raw\\index.json" $ encode pages
