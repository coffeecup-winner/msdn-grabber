{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module MsdnGrabber.MsdnTopic where

import Data.Aeson

data MsdnTopic = MsdnTopic { topicTitle :: String
                           , topicSections :: [MsdnTopicSection]
                           } deriving (Show)

data MsdnTopicSection = ContentSection { contentTitle :: String
                                       , contentBlocks :: [ContentBlock]
                                       }
                      | SeeAlsoSection { seeAlsoBlocks :: [SeeAlsoBlock] }
                      | UnknownSection
                      deriving (Show)

data ContentBlock = ParagraphBlock { paragraphText :: String }
                  | VerbatimBlock { verbatimText :: String }
                  | AlertBlock { alertText :: String }
                  | CaptionBlock { captionText :: String }
                  | TableBlock { tableContent :: [[String]] }
                  | ListBlock { listOrdered :: Bool
                              , listItems :: [String]
                              }
                  | DescriptionListBlock { dlList :: [(String, String)] }
                  | CodeBlock { codeText :: String }
                  | SubHeadingBlock { subHeadingText :: String }
                  | SubSectionBlock { subSectionBlocks :: [ContentBlock] }
                  | SectionBlock { sectionBlockSection :: MsdnTopicSection }
                  | UnknownBlock
                  deriving (Show)

data SeeAlsoBlock = SeeAlsoBlock { seeAlsoHeading :: String
                                 , seeAlsoLinks :: [MsdnLink]
                                 } deriving (Show)

data MsdnLink = MsdnLink { linkRef :: String
                         , linkText :: String
                         } deriving (Show)

instance ToJSON MsdnTopic where
  toJSON MsdnTopic{..} = object [ "title" .= topicTitle
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
