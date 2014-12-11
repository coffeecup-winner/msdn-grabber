module MsdnGrabber.MsdnTopic where

data MsdnTopic = MsdnTopic { topicFilename :: String
                           , topicTitle :: String
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
