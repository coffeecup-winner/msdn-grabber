{-# LANGUAGE OverloadedStrings #-}
module MsdnGrabber.HtmlSelectors where

import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

import Text.XML
import Text.XML.Cursor
import Text.XML.Scraping

nameIs :: T.Text -> Cursor -> Bool
nameIs name cursor = go $ node cursor
    where go (NodeElement element) = (== name) . nameLocalName . elementName $ element

classIs :: T.Text -> Cursor -> Bool
classIs name = (== name) . attr "class"

attr :: Name -> Cursor -> T.Text
attr name cursor = go $ node cursor
    where go (NodeElement element) = fromMaybe "" . Map.lookup name . elementAttributes $ element

text :: Cursor -> String
text = L.unpack . innerText

href :: Cursor -> String
href = T.unpack . attr "href"

a :: Axis
a = checkName (== "a")

p :: Axis
p = checkName (== "p")

h1 :: Axis
h1 = checkName (== "h1")

h2 :: Axis
h2 = checkName (== "h2")

div :: Axis
div = checkName (== "div")

span :: Axis
span = checkName (== "span")

pre :: Axis
pre = checkName (== "pre")

tr :: Axis
tr = checkName (== "tr")

(^) :: Axis -> T.Text -> Axis
a ^ name = a >=> if T.null name then check (null . hasAttribute "class") else attributeIs "class" name

(#) :: Axis -> T.Text -> Axis
a # name = a >=> attributeIs "id" name
