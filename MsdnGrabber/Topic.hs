module MsdnGrabber.Topic where

import Data.Text

data Topic = Topic { topicFilename :: Text
                   , topicTitle :: Text
                   , topicSections :: [Section]
                   } deriving (Show)

data Section = Section { sectionHeading :: Maybe Text
                       , sectionData :: SectionBlock
                       } deriving (Show)

data SectionBlock = Content { sectionContent :: [ContentBlock] }
                  | SubSections { subSections :: [Section] }
                  deriving (Show)

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
