{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module MsdnGrabber.Emit.Json where

import Data.Aeson

import MsdnGrabber.Topic

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
  toJSON (SeeAlsoBlock blocks) = object [ "type" .= ("see_also" :: String)
                                        , "blocks" .= blocks
                                        ]
  toJSON LinkBlock{..} = object [ "ref" .= linkRef
                                , "text" .= linkText
                                ]
  toJSON UnknownBlock = object [ "type" .= ("unknown" :: String) ]
