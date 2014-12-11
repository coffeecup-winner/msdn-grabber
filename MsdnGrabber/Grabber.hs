{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MsdnGrabber.Grabber where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as Set
import qualified Data.Text.Lazy.IO as LI
import Data.Tree

import Control.Applicative
import Control.Monad

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor
import Text.XML.Scraping
import Text.XML.Selector.TH

import Network (withSocketsDo)
import Network.HTTP.Conduit

import System.FilePath.Posix as Posix

import MsdnGrabber.WebPage
import MsdnGrabber.HtmlSelectors

withCursor :: String -> (Cursor -> IO a) -> IO a
withCursor url f = withSocketsDo $ do
    root <- (fromDocument . parseLBS) <$> simpleHttp url
    f root

findSubpages :: Cursor -> [Cursor]
findSubpages c = if not . null . queryT [jq| #tocnav > div.toclevel2.current |] $ c
    then []
    else (queryT [jq| #tocnav > div.toclevel2 |] &/ anyElement) c

generateRequest :: Cursor -> WebPageLink
generateRequest = WebPageLink <$> href <*> text

baseUrl :: String
baseUrl = "http://msdn.microsoft.com"

downloadPages :: String -> IO (Tree WebPageLink)
downloadPages url = do
    let request = WebPageLink url ""
    pages <- grab request
    save pages
    return pages

grab :: WebPageLink -> IO (Tree WebPageLink)
grab req = withCursor (baseUrl ++ wpLink req) $ \root -> do
    pages <- go $ root $// findSubpages &| generateRequest
    LI.writeFile (("raw\\" ++) . Posix.takeFileName . wpLink $ req) (innerHtml root)
    return $ newPage (wpLink req) (wpName req) pages
    where
        go :: [WebPageLink] -> IO [(Tree WebPageLink)]
        go rs = forM (removeDups rs) $ \r -> do
            putStrLn $ wpLink r ++ " " ++ wpName r
            grab r

save :: Tree WebPageLink -> IO ()
save pages = BL.writeFile "raw\\index.json" $ encode pages

removeDups :: Ord a => [a] -> [a]
removeDups = go Set.empty where
    go _ [] = []
    go s (x : xs) = if Set.member x s
        then go s xs
        else x : go (Set.insert x s) xs

