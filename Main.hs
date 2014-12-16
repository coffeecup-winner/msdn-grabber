{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, setFileSystemEncoding, setForeignEncoding, utf8)
import Options.Applicative

import MsdnGrabber.Grabber (downloadPages)
import MsdnGrabber.Parser (parsePages)
import MsdnGrabber.Emit.Json (saveJson)
import MsdnGrabber.Emit.Latex (saveLatex)

data Format = Json | LaTeX

instance Read Format where
    readsPrec _ v =
        tryParse [("json", Json)
                 ,("JSON", Json)
                 ,("tex", LaTeX)
                 ,("TeX", LaTeX)
                 ,("latex", LaTeX)
                 ,("LaTeX", LaTeX)
                 ]
        where tryParse [] = []
              tryParse ((attempt, result):xs) =
                  if (take (length attempt) v) == attempt
                     then [(result, drop (length attempt) v)]
                     else tryParse xs

data Options = Options { format :: Format
                       , root :: String
                       , threadsCount :: Int
                       }

options :: Parser Options
options = Options
    <$> option auto
        ( long "format"
       <> short 'f'
       <> metavar "FORMAT"
       <> value LaTeX
       <> help "Output format for the grabber (defaults to tex)" )
    <*> strOption
        ( long "page"
       <> short 'p'
       <> metavar "PAGE"
       <> help "Root page to grab" )
    <*> option auto
        ( long "threads"
       <> short 'n'
       <> metavar "THREADS"
       <> value 32
       <> help "Threads count (defaults to 32)" )

main :: IO ()
main = do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    setForeignEncoding utf8
    opts <- execParser parser
    runGrabber opts
    where parser = info (helper <*> options)
            ( fullDesc
           <> progDesc "Grab a MSDN PAGE and its children and convert them into FORMAT"
           <> header "MSDN Grabber v0.1" )

runGrabber :: Options -> IO ()
runGrabber Options{..} = do
    pages <- downloadPages threadsCount "/en-us/library/dd882498.aspx"
    topics <- parsePages pages
    case format of LaTeX -> saveLatex "topic.tex" topics
                   Json -> saveJson topics
