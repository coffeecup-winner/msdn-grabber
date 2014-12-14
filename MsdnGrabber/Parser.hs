{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module MsdnGrabber.Parser where

import Prelude hiding (div, span, (^))

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Traversable
import Data.Tree

import Control.Applicative

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor

import System.FilePath.Posix as Posix

import MsdnGrabber.HtmlSelectors
import MsdnGrabber.Topic
import MsdnGrabber.Utilities
import MsdnGrabber.WebPage

parsePages :: Tree WebPageLink -> IO (Tree Topic)
parsePages pageTree = do
    forM pageTree $ \p -> do
      let filename = wpFilename p
      root <- (fromDocument . parseLBS) <$> BL.readFile ("raw\\" ++ filename)
      return $ head $ root $// div^"topic" &| parseTopic filename

parseTopic :: String -> Cursor -> Topic
parseTopic filename root = let
    topicFilename = Posix.takeBaseName filename
    topicTitle = head $ root $/ h1^"title" &| text
    topicSections = introductionSection : miscSections
    introductionSection = head $ root $// div#"mainBody" &/ div^"introduction" &| parseSectionContent "Introduction"
    miscSections = root $// div#"mainBody" &/ div^"" &| parseSection
    in Topic{..}

parseSection :: Cursor -> ContentBlock
parseSection root = let
    title = head $ root $// span^"LW_CollapsibleArea_Title" &| text
    in head $ root $// div^"sectionblock" &| parseSectionContent title

parseSectionContent :: String -> Cursor -> ContentBlock
parseSectionContent title root = SectionBlock title $ root $/ anyElement >=> check (not . ("Toggle" `T.isInfixOf`) . attr "id") &| parseContentBlock

parseContentBlock :: Cursor -> ContentBlock
parseContentBlock elem | nameIs "p" elem = ParagraphBlock $ text elem
                       | nameIs "pre" elem = VerbatimBlock $ text elem
                       | classIs "alert" elem = AlertBlock $ head $ elem $// p &| text
                       | classIs "caption" elem = CaptionBlock $ text elem
                       | classIs "tableSection" elem = TableBlock $ elem $/ table &/ tr &| ($/ anyElement &| text)
                       | nameIs "ol" elem = ListBlock True $ elem $// p &| text
                       | nameIs "ul" elem = ListBlock False $ elem $// p &| text
                       | classIs "codeSnippetContainer" elem = CodeBlock $ head $ elem $// pre &| text
                       | classIs "authored" elem = DescriptionListBlock $ mapTuple text <$> groupBy2 (elem $/ anyElement)
                       | classIs "subHeading" elem = SubHeadingBlock $ text elem
                       | classIs "subsection" elem = SubSectionBlock $ elem $/ anyElement &| parseContentBlock
                       | classIs "seeAlsoNoToggleSection" elem = SubSectionBlock $ elem $/ anyElement &| parseContentBlock
                       | classIs "seeAlsoStyle" elem = LinkBlock <$> takeBaseName . href <*> text $ head $ elem $// a
                       | nameIs "div" elem && classIs "" elem = parseSection elem
                       | otherwise = UnknownBlock
