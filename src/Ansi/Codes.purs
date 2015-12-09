-- | This module defines a data type representing ANSI escape codes, as well as
-- | functions for serialising them as Strings.
module Ansi.Codes where 

import Prelude
import Data.String as String

-- | The prefix for all escape codes.
prefix :: String
prefix = "\x1b["

-- | The suffix for escape codes; note that this is only required for colors.
colorSuffix :: String
colorSuffix = "m"

-- | An ANSI escape code. Not all sequences are implemented. 
-- | See: <https://en.wikipedia.org/wiki/ANSI_escape_code>.
data EscapeCode
  = Up Int
  | Down Int
  | Forward Int
  | Back Int
  | NextLine Int
  | PreviousLine Int
  | HorizontalAbsolute Int
  | Position Int Int
  | EraseData EraseParam
  | EraseLine EraseParam
  | ScrollUp Int
  | ScrollDown Int
  | Graphics (Array GraphicsParam)
  | SavePosition
  | RestorePosition
  | QueryPosition
  | HideCursor
  | ShowCursor

-- | Convert an escape code to the form recognised by terminals.
escapeCodeToString :: EscapeCode -> String
escapeCodeToString = (prefix <>) <<< go
  where
  go c =
    case c of
      Up n                 -> show n <> "A"
      Down n               -> show n <> "B"
      Forward n            -> show n <> "C"
      Back n               -> show n <> "D"
      NextLine n           -> show n <> "E"
      PreviousLine n       -> show n <> "F"
      HorizontalAbsolute n -> show n <> "G"
      Position x y         -> show x <> ";" <> show y <> "H"
      EraseData p          -> ep p <> "J"
      EraseLine p          -> ep p <> "K"
      ScrollUp n           -> show n <> "S"
      ScrollDown n         -> show n <> "T"
      Graphics ps          -> String.joinWith ";" (map gp ps) <> colorSuffix
      SavePosition         -> "s"
      RestorePosition      -> "u"
      QueryPosition        -> "6n"
      HideCursor           -> "?25l"
      ShowCursor           -> "?25h"

  ep = eraseParamToString
  gp = graphicsParamToString

-- | Specifies how much text to erase.
-- |
-- | * ToEnd: erase from the cursor to the end of the line or screen.
-- | * FromBeginning: erase to the cursor from the beginning of the line or
-- |    screen.
-- | * Entire: erase the entire line or screen.
data EraseParam
  = ToEnd
  | FromBeginning
  | Entire

eraseParamToString :: EraseParam -> String
eraseParamToString ep =
  case ep of
    ToEnd         -> "0"
    FromBeginning -> "1"
    Entire        -> "2"

-- | A graphics parameter, controls how text appears; for example, bold,
-- | underlined, foreground color, background color.
data GraphicsParam
  = Reset
  | PMode RenderingMode
  | PForeground Color
  | PBackground Color

graphicsParamToString :: GraphicsParam -> String
graphicsParamToString gp =
  case gp of
    Reset         -> "0"
    PMode m       -> show (codeForRenderingMode m)
    PForeground c -> show (colorCode c)
    PBackground c -> show (colorCode c + 10)

data RenderingMode
  = Bold
  | Italic
  | Underline

codeForRenderingMode :: RenderingMode -> Int
codeForRenderingMode m =
  case m of
    Bold -> 1
    Italic -> 3
    Underline -> 4

-- | The standard set of 16 ANSI colors.
data Color
  = White
  | Black
  | Blue
  | Cyan
  | Green
  | Magenta
  | Red
  | Yellow
  | Grey
  | BrightBlack
  | BrightRed
  | BrightGreen
  | BrightYellow
  | BrightBlue
  | BrightMagenta
  | BrightCyan
  | BrightWhite

colorCode :: Color -> Int
colorCode c =
  case c of
    White -> 37
    Black -> 30
    Blue -> 34
    Cyan -> 36
    Green -> 32
    Magenta -> 35
    Red -> 31
    Yellow -> 33
    Grey -> 90
    BrightBlack -> 90
    BrightRed -> 91
    BrightGreen -> 92
    BrightYellow -> 93
    BrightBlue -> 94
    BrightMagenta -> 95
    BrightCyan -> 96
    BrightWhite -> 97
