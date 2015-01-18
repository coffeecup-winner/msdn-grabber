{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module MsdnGrabber.Parser where

import Prelude hiding (div, span, (^))

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import Data.Traversable
import Data.Tree

import Control.Applicative
import Control.Monad (join)

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor
import Text.XML hiding (parseLBS)

import MsdnGrabber.HtmlSelectors
import MsdnGrabber.Topic
import MsdnGrabber.Utilities
import MsdnGrabber.WebPage

parsePages :: Tree WebPageLink -> IO (Tree Topic)
parsePages pageTree =
    forM pageTree $ \p -> do
      let filename = wpFilename p
      root <- (fromDocument . parseLBS) <$> BL.readFile ("raw\\" ++ T.unpack filename)
      return $ head $ root $// div^"topic" &| parseTopic filename

parseTopic :: T.Text -> Cursor -> Topic
parseTopic filename root = let
    topicFilename = takeBaseName filename
    topicTitle = head $ root $/ h1^"title" &| text
    topicSections = introductionSection : miscSections
    introductionSection = head $ root $// div#"mainBody" &/ div^"introduction" &| parseSection Nothing
    miscSections = root $// div#"mainBody" &/ div^"" &| parseNamedSection
    in Topic{..}

parseNamedSection :: Cursor -> Section
parseNamedSection root = let
    title = head $ root $// span^"LW_CollapsibleArea_Title" &| text
    in head $ root $// div^"sectionblock" &| parseSection (Just title)

parseSection :: Maybe T.Text -> Cursor -> Section
parseSection title root = if any isSubSection elems
  then Section title . SubSections $ condMap isSubSection (\es -> Section Nothing . Content . join $ parseContent <$> es) parseNamedSection elems
  else Section title . Content . join $ parseContent <$> elems
  where
    elems = root $/ anyElement >=> check (not . ("Toggle" `T.isInfixOf`) . attr "id")
    isSubSection e = nameIs "div" e && classIs "" e

parseContent :: Cursor -> [ContentBlock]
parseContent elem | nameIs "p" elem = return $ ParagraphBlock $ parseParagraph elem
                  | nameIs "span" elem = return $ ParagraphBlock $ parseParagraph elem
                  | nameIs "pre" elem = return $ VerbatimBlock $ text elem
                  | classIs "alert" elem = return $ AlertBlock $ head $ elem $// p &| parseParagraph
                  | classIs "caption" elem = return $ CaptionBlock $ text elem
                  | classIs "tableSection" elem = return $ TableBlock $ elem $/ table &/ tr &| ($/ anyElement &| text)
                  | nameIs "ol" elem = return $ ListBlock True $ elem $// p &| ListItem . parseParagraph
                  | nameIs "ul" elem = return $ ListBlock False $ elem $// p &| ListItem . parseParagraph
                  | classIs "codeSnippetContainer" elem = return $ CodeBlock $ head $ elem $// pre &| text
                  | classIs "authored" elem = return $ DescriptionListBlock $ (\(t, d) -> DescriptionListItem (parseParagraph t) (d $/ anyElement &| head . parseContent)) <$> groupBy2 (elem $/ anyElement)
                  | classIs "subHeading" elem = return $ SubHeadingBlock $ text elem
                  | classIs "subsection" elem = join $ elem $/ anyElement &| parseContent
                  | classIs "seeAlsoNoToggleSection" elem = join $ elem $/ anyElement &| parseContent
                  | classIs "seeAlsoStyle" elem = return $ LinkBlock <$> takeBaseName . href <*> text $ head $ elem $// a
                  | nameIs "div" elem && classIs "" elem = []
                  | otherwise = return $ UnknownBlock

parseParagraph :: Cursor -> Paragraph
parseParagraph = go . node where
  go (NodeElement (Element "p" _ ns)) = join $ go <$> ns
  go (NodeElement (Element "dt" _ ns)) = join $ go <$> ns
  go (NodeElement (Element "a" as ns)) = (makeLink . fromJust . Map.lookup "href" $ as) <$> (go . head $ ns)
                                       where
                                         makeLink l (PlainText t) = RefText t (takeBaseName l)
  go (NodeElement (Element "span" as ns)) | isNothing cls = join $ go <$> ns
                                          | isJust cls = makeSpecial (fromJust cls) <$> (go . head $ ns)
                                          where
                                            cls = Map.lookup "class" as
  go (NodeElement (Element t _ ns)) = (makeSpecial . nameLocalName $ t) <$> (go . head $ ns)
  go (NodeContent t) = if T.null . T.strip $ t then [] else [PlainText t]
  go _ = []

makeSpecial :: T.Text -> TextBlock -> TextBlock
makeSpecial "input" (PlainText t) = BoldText t
makeSpecial "label" (PlainText t) = BoldText t
makeSpecial "strong" (PlainText t) = BoldText t
makeSpecial "parameter" (PlainText t) = ItalicText t
makeSpecial "code" (PlainText t) = MonospaceText t
makeSpecial "unresolvedLink" t = t
makeSpecial n (PlainText t) = UnknownText t n
