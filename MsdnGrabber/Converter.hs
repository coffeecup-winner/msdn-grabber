{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module MsdnGrabber.Converter where

import Prelude hiding (div, span, (^))

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LI
import qualified Data.Text.IO as TI
import Data.Tree

import Control.Applicative

import Text.HTML.DOM (parseLBS)
import Text.XML hiding (parseLBS)
import Text.XML.Cursor
import Text.XML.Scraping

import MsdnGrabber.HtmlSelectors
import MsdnGrabber.MsdnTopic
import MsdnGrabber.Utilities
import MsdnGrabber.WebPage

processPages :: WebPage -> IO ()
processPages page = do
    let count = getSum $ foldMap (const $ Sum 1) page
    forM_ (zip [1..] $ flatten page) $ \(i, p) -> do
      let filename = wpFilename p
      root <- (fromDocument . parseLBS) <$> BL.readFile ("raw\\" ++ filename)
      putStrLn $ "[" ++ show i ++ "\\" ++ show count ++ "] Processing " ++ filename
      let topic = head $ root $// div^"topic" &| parseTopic
      BL.writeFile ("data\\" ++ filename) $ encodePretty topic

parseTopic :: Cursor -> MsdnTopic
parseTopic root = let
    topicTitle = head $ root $/ h1^"title" &| text
    topicSections = introductionSection : miscSections
    introductionSection = head $ root $// div#"mainBody" &/ div^"introduction" &| parseSectionContent "Introduction"
    miscSections = root $// div#"mainBody" &/ div^"" &| parseSection
    in MsdnTopic{..}

parseSection :: Cursor -> MsdnTopicSection
parseSection root = let
    title = head $ root $// span^"LW_CollapsibleArea_Title" &| text
    in head $ root $// div^"sectionblock" &| parseSectionContent title

parseSectionContent :: String -> Cursor -> MsdnTopicSection
parseSectionContent title root | title == "See Also" = SeeAlsoSection $ getSeeAlsoBlocks $ root $/ anyElement
                               | otherwise = ContentSection title $ root $/ anyElement >=> check (not . ("Toggle" `T.isInfixOf`) . attr "id") &| parseContentBlock
    where getSeeAlsoBlocks elems = (SeeAlsoBlock <$> text . fst <*> fmap link . snd) <$> groupBy (nameIs "h4") elems
          link cursor = MsdnLink <$> href <*> text $ head $ cursor $// a

parseContentBlock :: Cursor -> ContentBlock
parseContentBlock elem | nameIs "p" elem = ParagraphBlock $ text elem
                       | nameIs "pre" elem = VerbatimBlock $ text elem
                       | classIs "alert" elem = AlertBlock $ head $ elem $// p &| text
                       | classIs "caption" elem = CaptionBlock $ text elem
                       | classIs "tableSection" elem = TableBlock $ elem $// tr &| ($// p &| text)
                       | nameIs "ol" elem = ListBlock True $ elem $// p &| text
                       | nameIs "ul" elem = ListBlock False $ elem $// p &| text
                       | classIs "codeSnippetContainer" elem = CodeBlock $ head $ elem $// pre &| text
                       | classIs "authored" elem = DescriptionListBlock $ mapTuple text <$> groupBy2 (elem $/ anyElement)
                       | classIs "subHeading" elem = SubHeadingBlock $ text elem
                       | classIs "subsection" elem = SubSectionBlock $ elem $/ anyElement &| parseContentBlock
                       | classIs "seeAlsoNoToggleSection" elem = SubSectionBlock $ elem $/ anyElement &| parseContentBlock
                       | nameIs "div" elem && classIs "" elem = SectionBlock $ parseSection elem
                       | otherwise = UnknownBlock
