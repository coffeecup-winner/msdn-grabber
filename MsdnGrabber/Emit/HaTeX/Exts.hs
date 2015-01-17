{-# LANGUAGE OverloadedStrings #-}
module MsdnGrabber.Emit.HaTeX.Exts where

import Data.Matrix
import qualified Data.List as List

import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Texy
import Text.LaTeX.Base.Types

tabularX :: PackageName
tabularX = "tabularx"

-- | The 'tabularx' environment can be used to typeset tables with optional horizontal and vertical lines.
tabularx :: LaTeXC l =>
            Maybe Pos   -- ^ This optional parameter can be used to specify the vertical position of the table.
                        --   Defaulted to 'Center'.
         -> Measure
         -> [TableSpecX] -- ^ Table specification of columns and vertical lines.
         -> l       -- ^ Table content. See '&', 'lnbk', 'hline' and 'cline'.
         -> l       -- ^ Resulting table syntax.
tabularx Nothing w ts  = liftL $ TeXEnv "tabularx" [ FixArg $ TeXRaw $ render w, FixArg $ TeXRaw $ renderAppend ts ]
tabularx (Just p) w ts = liftL $ TeXEnv "tabularx" [ OptArg $ TeXRaw $ render p, FixArg $ TeXRaw $ render w, FixArg $ TeXRaw $ renderAppend ts ]

matrixTabularx :: (LaTeXC l, Texy a)
               => Measure
               -> [TableSpecX] -- ^ Table specification of columns.
               -> [l] -- ^ (Non-empty) List of column titles
               -> Matrix a -- ^ Matrix of data
               -> l -- ^ Data organized in a tabular environment
matrixTabularx w tspec ts m =
  let spec = VerticalLineX : List.intersperse VerticalLineX tspec ++ [VerticalLineX]
  in  tabularx Nothing w spec $ mconcat
        [ hline
        , foldl1 (&) ts
        , lnbk
        , hline
        , mconcat $ fmap (
            \i -> mconcat [ foldl1 (&) $ fmap (\j -> texy (m ! (i,j))) [1 .. ncols m]
                          , lnbk
                          , hline
                            ] ) [1 .. nrows m]
          ]

-- | Type of table specifications.
data TableSpecX =
   LeftColumnX         -- ^ Left-justified column.
 | CenterColumnX       -- ^ Centered column.
 | RightColumnX        -- ^ Right-justified column.
 | AutoColumnX         -- ^ Automatic-width column.
 | ParColumnTopX LaTeX -- ^ Paragraph column with text vertically aligned at the top.
 | ParColumnMidX LaTeX -- ^ Paragraph column with text vertically aligned at the middle. Requires 'array' package.
 | ParColumnBotX LaTeX -- ^ Paragraph column with text vertically aligned at the bottom. Requires 'array' package.
 | VerticalLineX       -- ^ Vertical line between two columns.
 | DVerticalLineX      -- ^ Double vertical line between two columns.
 | SeparatorX LaTeX    -- ^ Column separator. Requires 'array' package.
   deriving Show

instance Render TableSpecX where
 render LeftColumnX       = "l"
 render CenterColumnX     = "c"
 render RightColumnX      = "r"
 render AutoColumnX       = "X"
 render (ParColumnTopX l) = "p" <> render (FixArg l)
 render (ParColumnMidX l) = "m" <> render (FixArg l)
 render (ParColumnBotX l) = "b" <> render (FixArg l)
 render VerticalLineX     = "|"
 render DVerticalLineX    = "||"
 render (SeparatorX l)    = "@" <> render (FixArg l)

tabularY :: PackageName
tabularY = "tabulary"

-- | The 'tabularx' environment can be used to typeset tables with optional horizontal and vertical lines.
tabulary :: LaTeXC l =>
            Maybe Pos   -- ^ This optional parameter can be used to specify the vertical position of the table.
                        --   Defaulted to 'Center'.
         -> Measure
         -> [TableSpecY] -- ^ Table specification of columns and vertical lines.
         -> l       -- ^ Table content. See '&', 'lnbk', 'hline' and 'cline'.
         -> l       -- ^ Resulting table syntax.
tabulary Nothing w ts  = liftL $ TeXEnv "tabulary" [ FixArg $ TeXRaw $ render w, FixArg $ TeXRaw $ renderAppend ts ]
tabulary (Just p) w ts = liftL $ TeXEnv "tabulary" [ OptArg $ TeXRaw $ render p, FixArg $ TeXRaw $ render w, FixArg $ TeXRaw $ renderAppend ts ]

matrixTabulary :: (LaTeXC l, Texy a)
               => Measure
               -> [TableSpecY] -- ^ Table specification of columns.
               -> [l] -- ^ (Non-empty) List of column titles
               -> Matrix a -- ^ Matrix of data
               -> l -- ^ Data organized in a tabular environment
matrixTabulary w tspec ts m =
  let spec = VerticalLineY : List.intersperse VerticalLineY tspec ++ [VerticalLineY]
  in  tabulary Nothing w spec $ mconcat
        [ hline
        , foldl1 (&) ts
        , lnbk
        , hline
        , mconcat $ fmap (
            \i -> mconcat [ foldl1 (&) $ fmap (\j -> texy (m ! (i,j))) [1 .. ncols m]
                          , lnbk
                          , hline
                            ] ) [1 .. nrows m]
          ]

-- | Type of table specifications.
data TableSpecY =
   LeftColumnY         -- ^ Left-justified column.
 | CenterColumnY       -- ^ Centered column.
 | RightColumnY        -- ^ Right-justified column.
 | JustifyColumnY      -- ^ Automatic-width column.
 | ParColumnTopY LaTeX -- ^ Paragraph column with text vertically aligned at the top.
 | ParColumnMidY LaTeX -- ^ Paragraph column with text vertically aligned at the middle. Requires 'array' package.
 | ParColumnBotY LaTeX -- ^ Paragraph column with text vertically aligned at the bottom. Requires 'array' package.
 | VerticalLineY       -- ^ Vertical line between two columns.
 | DVerticalLineY      -- ^ Double vertical line between two columns.
 | SeparatorY LaTeX    -- ^ Column separator. Requires 'array' package.
   deriving Show

instance Render TableSpecY where
 render LeftColumnY       = "L"
 render CenterColumnY     = "L"
 render RightColumnY      = "R"
 render JustifyColumnY    = "J"
 render (ParColumnTopY l) = "p" <> render (FixArg l)
 render (ParColumnMidY l) = "m" <> render (FixArg l)
 render (ParColumnBotY l) = "b" <> render (FixArg l)
 render VerticalLineY     = "|"
 render DVerticalLineY    = "||"
 render (SeparatorY l)    = "@" <> render (FixArg l)
