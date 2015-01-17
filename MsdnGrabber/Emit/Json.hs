{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module MsdnGrabber.Emit.Json where

import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Tree

import System.Directory (createDirectoryIfMissing)

import MsdnGrabber.Topic

saveJson :: Tree Topic -> IO ()
saveJson topics = do
  createDirectoryIfMissing False "data"
  forM_ (flatten topics) $ \t -> do
    let filename = topicFilename t
    BL.writeFile ("data\\" ++ T.unpack filename) $ encodePretty t

instance ToJSON Topic where
  toJSON Topic{..} = object [ "filename" .= topicFilename
                            , "title" .= topicTitle
                            , "sections" .= topicSections
                            ]

instance ToJSON ContentBlock where
  toJSON (ParagraphBlock text) = object [ "type" .= ("paragraph" :: String)
                                        , "text" .= text
                                        ]
  toJSON (VerbatimBlock text) = object [ "type" .= ("verbatim" :: String)
                                       , "text" .= text
                                       ]
  toJSON (CaptionBlock text) = object [ "type" .= ("caption" :: String)
                                      , "text" .= text
                                      ]
  toJSON (AlertBlock text) = object [ "type" .= ("alert" :: String)
                                    , "text" .= text
                                    ]
  toJSON (TableBlock table) = object [ "type" .= ("table" :: String)
                                     , "table" .= table
                                     ]
  toJSON ListBlock{..} = object [ "type" .= ("list" :: String)
                                , "ordered" .= listOrdered
                                , "items" .= listItems
                                ]
  toJSON (DescriptionListBlock list) = object [ "type" .= ("description_list" :: String)
                                              , "list" .= list
                                              ]
  toJSON (CodeBlock code) = object [ "type" .= ("code" :: String)
                                   , "code" .= code
                                   ]
  toJSON (SubHeadingBlock text) = object [ "type" .= ("subheading" :: String)
                                         , "text" .= text
                                         ]
  toJSON (SubSectionBlock blocks) = object [ "type" .= ("subsection" :: String)
                                           , "blocks" .= blocks
                                           ]
  toJSON (SectionBlock title blocks) = object [ "type" .= ("section" :: String)
                                              , "title" .= title
                                              , "blocks" .= blocks
                                              ]
  toJSON LinkBlock{..} = object [ "ref" .= linkRef
                                , "text" .= linkText
                                ]
  toJSON UnknownBlock = object [ "type" .= ("unknown" :: String) ]

instance ToJSON DescriptionListItem where
  toJSON (DescriptionListItem term desc) = object [ "term" .= term
                                        , "desc" .= desc
                                        ]

instance ToJSON ListItem where
  toJSON (ListItem par) = toJSON par

instance ToJSON TextBlock where
  toJSON (PlainText text) = object [ "plain" .= text  ]
  toJSON (BoldText text) = object [ "bold" .= text  ]
  toJSON (ItalicText text) = object [ "italic" .= text  ]
  toJSON (MonospaceText text) = object [ "monospace" .= text ]
  toJSON (RefText text ref) = object [ "text" .= text
                                     , "ref" .= ref
                                     ]
  toJSON (UnknownText text cls) = object [ "text" .= text
                                         , "class" .= cls
                                         ]
