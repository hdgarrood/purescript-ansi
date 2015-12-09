
module Ansi.Codes where 

import Prelude hiding (Show)
import Data.Set (Set())
import Data.Set as Set
import Data.List as List
import Data.String as String

-- | The prefix for all escape codes
prefix :: String
prefix = "\x1b["

-- | The suffix for escape codes; note that this is only required for colors.
colorSuffix :: String
colorSuffix = "m"

-- | An ANSI escape sequence. Not all sequences are implemented. 
-- | See: https://en.wikipedia.org/wiki/ANSI_escape_code.
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
  | Graphics (Set GraphicsParam)
  | SavePosition
  | RestorePosition
  | QueryPosition
  | Hide
  | Show

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
      Graphics ps          -> String.joinWith ";" (map gp (toArray ps))
      SavePosition         -> "s"
      RestorePosition      -> "u"
      QueryPosition        -> "6n"
      Hide                 -> "?25l"
      Show                 -> "?25h"

  ep = eraseParamToString

  toArray :: forall a. Set a -> Array a
  toArray = Set.toList >>> List.fromList

  gp = graphicsParamToString

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
  | Inverse

codeForRenderingMode :: RenderingMode -> Int
codeForRenderingMode m =
  case m of
    Bold -> 1
    Italic -> 3
    Underline -> 4
    Inverse -> 7

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
