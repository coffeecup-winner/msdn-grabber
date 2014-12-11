module MsdnGrabber.Topic where

data Topic = Topic { topicFilename :: String
                   , topicTitle :: String
                   , topicSections :: [ContentBlock]
                   } deriving (Show)

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
                  | SectionBlock { sectionTitle :: String
                                 , sectionContent :: [ContentBlock]
                                 }
                  | SeeAlsoBlock { seeAlsoBlocks :: [ContentBlock] }
                  | LinkBlock { linkRef :: String
                              , linkText :: String
                              }
                  | UnknownBlock
                  deriving (Show)
