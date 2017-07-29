-- | Convenience functions to simplify outputting ANSI escape codes to
-- | terminals.
module Ansi.Output where

import Prelude

import Ansi.Codes (Color, EscapeCode(..), GraphicsParam(..), RenderingMode(..), escapeCodeToString)

-- | Wrap the given text in escape codes corresponding to the given parameters.
-- | For example:
-- |
-- | ```purescript
-- | Console.log $ withGraphics (bold <> underline <> foreground BrightRed) "hello world"
-- | ```
-- |
-- | would print "hello world" to the terminal, bold, underlined, and in bright
-- | red, and then reset (so that further logging to the console uses the
-- | normal color and style).
-- |
-- | This function simply wraps the given text in an escape code and a reset
-- | code, so that it is a little more comfortable to use than the functions
-- | in `Ansi.Codes`.
withGraphics :: Array GraphicsParam -> String -> String
withGraphics params text =
  escapeCodeToString (Graphics params) <>
  text <>
  escapeCodeToString (Graphics [Reset])

bold :: Array GraphicsParam
bold = [PMode Bold]

dim :: Array GraphicsParam
dim = [PMode Dim]

italic :: Array GraphicsParam
italic = [PMode Italic]

underline :: Array GraphicsParam
underline = [PMode Underline]

inverse :: Array GraphicsParam
inverse = [PMode Inverse]

strikethrough :: Array GraphicsParam
strikethrough = [PMode Strikethrough]

foreground :: Color -> Array GraphicsParam
foreground c = [PForeground c]

background :: Color -> Array GraphicsParam
background c = [PBackground c]
