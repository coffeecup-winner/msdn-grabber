{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module MsdnGrabber.Emit.Json where

import Data.Aeson

import MsdnGrabber.MsdnTopic

instance ToJSON MsdnTopic where
  toJSON MsdnTopic{..} = object [ "filename" .= topicFilename
                                , "title" .= topicTitle
                                , "sections" .= topicSections
                                ]

instance ToJSON MsdnTopicSection where
  toJSON (ContentSection title blocks) = object [ "type" .= ("content" :: String)
                                                , "title" .= title
                                                , "blocks" .= blocks
                                                ]
  toJSON (SeeAlsoSection blocks) = object [ "type" .= ("see_also" :: String)
                                          , "blocks" .= blocks
                                          ]
  toJSON UnknownSection = object [ "type" .= ("unknown" :: String) ]

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
  toJSON (SectionBlock section) = object [ "type" .= ("section" :: String)
                                         , "section" .= section
                                         ]
  toJSON UnknownBlock = object [ "type" .= ("unknown" :: String) ]

instance ToJSON SeeAlsoBlock where
  toJSON SeeAlsoBlock{..} = object [ "heading" .= seeAlsoHeading
                                   , "links" .= seeAlsoLinks
                                   ]

instance ToJSON MsdnLink where
  toJSON MsdnLink{..} = object [ "ref" .= linkRef
                               , "text" .= linkText
                               ]
