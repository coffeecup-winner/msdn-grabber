{-# LANGUAGE OverloadedStrings #-}
module MsdnGrabber.Emit.Latex where

import Control.Applicative
import Control.Monad
import qualified Data.List as List
import Data.Matrix
import Data.Tree

import Text.LaTeX
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Hyperref
import Text.LaTeX.Packages.Inputenc
import MsdnGrabber.Emit.HaTeX.Exts

import MsdnGrabber.Topic

saveLatex :: String -> Tree Topic -> IO ()
saveLatex filename topics = execLaTeXT (writeTopics topics) >>= renderFile filename

writeTopics :: Monad m => Tree Topic -> LaTeXT_ m
writeTopics topics = do
    preamble . topicTitle . rootLabel $ topics
    document $ body topics

preamble :: Monad m => String -> LaTeXT_ m
preamble t = do
    documentclass [a4paper, openany] report
    importGeometry [GWidth $ Pt 450, GHeight $ Pt 700]
    usepackage [] hyperref
    usepackage [utf8] inputenc
    usepackage [] tabularY
    raw "\\DeclareUnicodeCharacter{00A0}{~}"
    author "MsdnGrabber"
    title $ fromString t

body :: Monad m => Tree Topic -> LaTeXT_ m
body tree = do
    tableofcontents
    mapM_ (writeTopic 1) $ subForest tree

writeTopic :: Monad m => Int -> Tree Topic -> LaTeXT_ m
writeTopic n tree = do
    let topic = rootLabel tree
    sectionFromLevel n (fromString . topicTitle $ topic)
    label (fromString . topicFilename $ topic)
    mapM_ writeSection (flattenSections . topicSections $ topic)
    mapM_ (writeTopic (n + 1)) $ subForest tree

flattenSections :: [ContentBlock] -> [ContentBlock]
flattenSections [] = []
flattenSections (SubSectionBlock blocks : rest) = flattenSections blocks ++ flattenSections rest
flattenSections (SectionBlock title blocks : rest) = SubHeadingBlock title : flattenSections blocks ++ flattenSections rest
flattenSections (block : rest) = block : flattenSections rest

writeSection :: Monad m => ContentBlock -> LaTeXT_ m
writeSection (ParagraphBlock textBlocks) = writeTextBlocks textBlocks
writeSection (VerbatimBlock text) = verbatim $ fromString text
writeSection (AlertBlock textBlocks) = do
    textbf "NOTE"
    newline
    writeTextBlocks textBlocks
writeSection (CaptionBlock text) = writeString text
writeSection (TableBlock (headers:table)) = do
    matrixTabulary (Pt 450) (tspec headers) (fmap (textbf . fromString) headers) (fromLists texTable)
    newline
    where
        tspec :: [String] -> [TableSpecY]
        tspec hs = List.replicate (length hs) CenterColumnY
        texTable :: [[Text]]
        texTable = fmap fromString <$> table
writeSection (ListBlock ordered items) =
    (if ordered then enumerate else itemize) $
        forM_ items $ \textBlocks -> do
            item Nothing
            writeTextBlocks textBlocks
writeSection (DescriptionListBlock list) =
    forM_ list $ \(term, desc) -> do
        writeTextBlocks term
        forM_ desc writeSection
writeSection (CodeBlock text) = verbatim $ fromString text
writeSection (SubHeadingBlock text) = textbf (large $ fromString text) <> newline
writeSection (LinkBlock link _) = nameref (fromString link) <> newline
writeSection UnknownBlock = error "Unknown block!"

writeTextBlocks :: Monad m => [TextBlock] -> LaTeXT_ m
writeTextBlocks blocks = if null blocks then mempty else mconcat (writeTextBlock <$> blocks) <> newline

writeTextBlock :: Monad m => TextBlock -> LaTeXT_ m
writeTextBlock (PlainText t) = fromString t
writeTextBlock (BoldText t) = textbf $ fromString t
writeTextBlock (ItalicText t) = textit $ fromString t
writeTextBlock (MonospaceText t) = texttt $ fromString t
writeTextBlock (RefText t link) = if head link == '#' then fromString t else nameref $ fromString link
writeTextBlock (UnknownText _ _) = error "Unknown text!"

writeString :: Monad m => String -> LaTeXT_ m
writeString s = if null s then mempty else fromString s <> newline

sectionFromLevel :: Monad m => Int -> LaTeXT_ m -> LaTeXT_ m
sectionFromLevel 1 = chapter
sectionFromLevel 2 = section
sectionFromLevel 3 = subsection
sectionFromLevel 4 = subsubsection
sectionFromLevel _ = error "Unsupported level of nesting!"
