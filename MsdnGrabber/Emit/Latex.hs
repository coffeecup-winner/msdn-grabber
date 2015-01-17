{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module MsdnGrabber.Emit.Latex where

import Control.Applicative
import Control.Monad
import qualified Data.List as List
import Data.Matrix
import qualified Data.Text as T
import Data.Tree

import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.Geometry
import qualified Text.LaTeX.Packages.Hyperref as Hyperref
import Text.LaTeX.Packages.Inputenc
import MsdnGrabber.Emit.HaTeX.Exts

import MsdnGrabber.Topic

saveLatex :: String -> Tree Topic -> IO ()
saveLatex filename topics = execLaTeXT (writeTopics topics) >>= renderFile filename

writeTopics :: Monad m => Tree Topic -> LaTeXT_ m
writeTopics topics = do
    preamble . topicTitle . rootLabel $ topics
    document $ body topics

preamble :: Monad m => Text -> LaTeXT_ m
preamble t = do
    documentclass [a4paper, openany] report
    importGeometry [GWidth $ Pt 450, GHeight $ Pt 700]
    usepackage [] Hyperref.hyperref
    usepackage [utf8] inputenc
    usepackage [] tabularY
    raw "\\DeclareUnicodeCharacter{00A0}{~}"
    author "MsdnGrabber"
    title $ texy t

body :: Monad m => Tree Topic -> LaTeXT_ m
body tree = do
    tableofcontents
    mapM_ (writeTopic 1) $ subForest tree

writeTopic :: Monad m => Int -> Tree Topic -> LaTeXT_ m
writeTopic n tree = do
    let topic = rootLabel tree
    sectionFromLevel n (texy . topicTitle $ topic)
    label (texy . topicFilename $ topic)
    mapM_ texy (flattenSections . topicSections $ topic)
    mapM_ (writeTopic (n + 1)) $ subForest tree

sectionFromLevel :: LaTeXC l => Int -> l -> l
sectionFromLevel 1 = chapter
sectionFromLevel 2 = section
sectionFromLevel 3 = subsection
sectionFromLevel 4 = subsubsection
sectionFromLevel _ = error "Unsupported level of nesting!"

flattenSections :: [ContentBlock] -> [ContentBlock]
flattenSections [] = []
flattenSections (SubSectionBlock blocks : rest) = flattenSections blocks ++ flattenSections rest
flattenSections (SectionBlock title blocks : rest) = SubHeadingBlock title : flattenSections blocks ++ flattenSections rest
flattenSections (block : rest) = block : flattenSections rest

instance Texy l => Texy [l] where
    texy bs = mconcat (texy <$> bs)

instance (Texy a, Texy b) => Texy (a, b) where
    texy (a, b) = texy a <> texy b

instance Texy ContentBlock where
    texy (ParagraphBlock blocks) = texy blocks
    texy (VerbatimBlock text) = verbatim text
    texy (AlertBlock blocks) = textbf "NOTE" <> newline <> texy blocks
    texy (CaptionBlock text) = if T.null text then mempty else texy text <> newline
    texy (TableBlock (headers:table)) = matrixTabulary (Pt 450) (tspec headers) (fmap (textbf . texy) headers) (fromLists table) <> newline
        where
            tspec :: [Text] -> [TableSpecY]
            tspec hs = List.replicate (length hs) CenterColumnY
    texy (ListBlock ordered items) =
        (if ordered then enumerate else itemize) $ texy items
    texy (DescriptionListBlock list) = texy list
    texy (CodeBlock text) = verbatim text
    texy (SubHeadingBlock text) = textbf (large $ texy text) <> newline
    texy (LinkBlock link _) = nameref link <> newline
    texy UnknownBlock = error "Unknown block!"

instance Texy DescriptionListItem where
    texy (DescriptionListItem term desc) = texy term <> texy desc

instance Texy ListItem where
    texy (ListItem par) = item Nothing <> texy par <> newline

instance Texy Paragraph where
    texy par = if null par then mempty else mconcat (texy <$> par) <> newline

instance Texy TextBlock where
    texy (PlainText t) = texy t
    texy (BoldText t) = textbf $ texy t
    texy (ItalicText t) = textit $ texy t
    texy (MonospaceText t) = texttt $ texy t
    texy (RefText t link) = if T.head link == '#' then texy t else nameref link
    texy (UnknownText _ _) = error "Unknown text!"

nameref :: (LaTeXC l) => T.Text -> l
nameref = Hyperref.nameref . fromString . T.unpack
