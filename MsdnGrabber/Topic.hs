module MsdnGrabber.Topic where

import Data.Text

data Topic = Topic { topicFilename :: Text
                   , topicTitle :: Text
                   , topicSections :: [ContentBlock]
                   } deriving (Show)

data ContentBlock = ParagraphBlock { paragraphText :: Paragraph }
                  | VerbatimBlock { verbatimText :: Text }
                  | AlertBlock { alertText :: Paragraph }
                  | CaptionBlock { captionText :: Text }
                  | TableBlock { tableContent :: [[Text]] }
                  | ListBlock { listOrdered :: Bool
                              , listItems :: [ListItem]
                              }
                  | DescriptionListBlock { dlList :: [DescriptionListItem] }
                  | CodeBlock { codeText :: Text }
                  | SubHeadingBlock { subHeadingText :: Text }
                  | SubSectionBlock { subSectionBlocks :: [ContentBlock] }
                  | SectionBlock { sectionTitle :: Text
                                 , sectionContent :: [ContentBlock]
                                 }
                  | LinkBlock { linkRef :: Text
                              , linkText :: Text
                              }
                  | UnknownBlock
                  deriving (Show)

data DescriptionListItem = DescriptionListItem Paragraph [ContentBlock]
                         deriving (Show)

data ListItem = ListItem Paragraph
              deriving (Show)

type Paragraph = [TextBlock]

data TextBlock = PlainText { plainText :: Text }
               | BoldText { boldText :: Text }
               | ItalicText { italicText :: Text }
               | MonospaceText { monospaceText :: Text }
               | RefText { refText :: Text
                         , refLocation :: Text
                         }
               | UnknownText { unknownText :: Text
                             , unknownClass :: Text
                             }
               deriving (Show)
