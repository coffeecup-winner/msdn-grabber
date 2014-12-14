{-# LANGUAGE OverloadedStrings #-}
module MsdnGrabber.Emit.Latex where

import Control.Monad
import Data.Matrix
import qualified Data.Text as T
import Data.Tree

import Text.LaTeX
import Text.LaTeX.Packages.Inputenc

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types

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
    usepackage [] "hyperref"
    usepackage [utf8] inputenc
    author "MsdnGrabber"
    title $ fromString t

body :: Monad m => Tree Topic -> LaTeXT_ m
body topics = do
    maketitle
    tableofcontents
    forM_ (subForest topics) $ \t -> do
        writeTopic 1 t

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
writeSection (ParagraphBlock text) = do
    writeString text
writeSection (VerbatimBlock text) = do
    verbatim $ fromString text
writeSection (AlertBlock text) = do
    textbf "NOTE" <> (raw ("A" :: Text))
    newline
    writeString text
writeSection (CaptionBlock text) = do
    writeString text
writeSection (TableBlock (headers:table)) = do
    matrixTabular (fmap (textbf . fromString) headers) (fromLists texTable)
    newline
    where
        texTable :: [[Text]]
        texTable = fmap (fmap fromString) $ table
writeSection (ListBlock ordered items) = do
    (if ordered then enumerate else itemize) $ do
        forM_ items $ \i -> do
            item Nothing
            fromString i
writeSection (DescriptionListBlock list) = do
    forM_ list $ \(term, desc) -> do
        fromString term
        writeString desc
writeSection (CodeBlock text) = do
    verbatim $ fromString text
writeSection (SubHeadingBlock text) = do
    textbf (large $ fromString text)
    newline
writeSection (LinkBlock link text) = do
    nameref (fromString link)
    newline
writeSection UnknownBlock = error "Unknown block!"

writeString :: Monad m => String -> LaTeXT_ m
writeString s = if null s then mempty else fromString s <> newline

sectionFromLevel :: Monad m => Int -> (LaTeXT_ m -> LaTeXT_ m)
sectionFromLevel 1 = chapter
sectionFromLevel 2 = section
sectionFromLevel 3 = subsection
sectionFromLevel 4 = subsubsection

nameref :: LaTeXC l => Label -> l
nameref l = fromLaTeX $ TeXComm "nameref" [ FixArg $ rendertex l ]
