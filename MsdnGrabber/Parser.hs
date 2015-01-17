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
    introductionSection = head $ root $// div#"mainBody" &/ div^"introduction" &| parseSectionContent "Introduction"
    miscSections = root $// div#"mainBody" &/ div^"" &| parseSection
    in Topic{..}

parseSection :: Cursor -> ContentBlock
parseSection root = let
    title = head $ root $// span^"LW_CollapsibleArea_Title" &| text
    in head $ root $// div^"sectionblock" &| parseSectionContent title

parseSectionContent :: T.Text -> Cursor -> ContentBlock
parseSectionContent title root = SectionBlock title $ root $/ anyElement >=> check (not . ("Toggle" `T.isInfixOf`) . attr "id") &| parseContentBlock

parseContentBlock :: Cursor -> ContentBlock
parseContentBlock elem | nameIs "p" elem = ParagraphBlock $ parseParagraph elem
                       | nameIs "span" elem = ParagraphBlock $ parseParagraph elem
                       | nameIs "pre" elem = VerbatimBlock $ text elem
                       | classIs "alert" elem = AlertBlock $ head $ elem $// p &| parseParagraph
                       | classIs "caption" elem = CaptionBlock $ text elem
                       | classIs "tableSection" elem = TableBlock $ elem $/ table &/ tr &| ($/ anyElement &| text)
                       | nameIs "ol" elem = ListBlock True $ elem $// p &| ListItem . parseParagraph
                       | nameIs "ul" elem = ListBlock False $ elem $// p &| ListItem . parseParagraph
                       | classIs "codeSnippetContainer" elem = CodeBlock $ head $ elem $// pre &| text
                       | classIs "authored" elem = DescriptionListBlock $ (\(t, d) -> DescriptionListItem (parseParagraph t) (d $/ anyElement &| parseContentBlock)) <$> groupBy2 (elem $/ anyElement)
                       | classIs "subHeading" elem = SubHeadingBlock $ text elem
                       | classIs "subsection" elem = SubSectionBlock $ elem $/ anyElement &| parseContentBlock
                       | classIs "seeAlsoNoToggleSection" elem = SubSectionBlock $ elem $/ anyElement &| parseContentBlock
                       | classIs "seeAlsoStyle" elem = LinkBlock <$> takeBaseName . href <*> text $ head $ elem $// a
                       | nameIs "div" elem && classIs "" elem = parseSection elem
                       | otherwise = UnknownBlock

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
