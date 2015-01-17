module MsdnGrabber.Topic where

data Topic = Topic { topicFilename :: String
                   , topicTitle :: String
                   , topicSections :: [ContentBlock]
                   } deriving (Show)

data ContentBlock = ParagraphBlock { paragraphText :: [TextBlock] }
                  | VerbatimBlock { verbatimText :: String }
                  | AlertBlock { alertText :: [TextBlock] }
                  | CaptionBlock { captionText :: String }
                  | TableBlock { tableContent :: [[String]] }
                  | ListBlock { listOrdered :: Bool
                              , listItems :: [[TextBlock]]
                              }
                  | DescriptionListBlock { dlList :: [([TextBlock], [ContentBlock])] }
                  | CodeBlock { codeText :: String }
                  | SubHeadingBlock { subHeadingText :: String }
                  | SubSectionBlock { subSectionBlocks :: [ContentBlock] }
                  | SectionBlock { sectionTitle :: String
                                 , sectionContent :: [ContentBlock]
                                 }
                  | LinkBlock { linkRef :: String
                              , linkText :: String
                              }
                  | UnknownBlock
                  deriving (Show)

data TextBlock = PlainText { plainText :: String }
               | BoldText { boldText :: String }
               | ItalicText { italicText :: String }
               | MonospaceText { monospaceText :: String }
               | RefText { refText :: String
                         , refLocation :: String
                         }
               | UnknownText { unknownText :: String
                             , unknownClass :: String
                             }
               deriving (Show)
