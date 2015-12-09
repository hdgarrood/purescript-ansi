-- | Convenience functions to simplify outputting ANSI escape codes to
-- | terminals.
module Ansi.Output where

import Prelude

import Ansi.Codes

-- | Using the given output function, write the given text with the given set
-- | of graphics parameters. For example:
-- |
-- | ```purescript
-- | withGraphics Console.log (bold <> underline <> foreground BrightRed) "hello world"
-- | ```
-- |
-- | would print "hello world" to the terminal, bold, underlined, and in bright
-- | red, and then reset (so that further logging to the console uses the
-- | normal color and style).
-- |
-- | This function works by first printing the escape code corresponding to the
-- | supplied graphics parameters, then printing the text, and then printing a
-- | reset escape code.
-- |
-- | The first argument should take a string, and write it to some output stream
-- | such as a terminal. In most cases, you will want to use either
-- | `Console.log` or `Console.error`.
withGraphics :: forall m. (Monad m) => (String -> m Unit) -> Array GraphicsParam -> String -> m Unit
withGraphics write params text = do
  write (escapeCodeToString (Graphics params))
  write text
  write (escapeCodeToString (Graphics [Reset]))

bold :: Array GraphicsParam
bold = [PMode Bold]

italic :: Array GraphicsParam
italic = [PMode Italic]

underline :: Array GraphicsParam
underline = [PMode Underline]

foreground :: Color -> Array GraphicsParam
foreground c = [PForeground c]

background :: Color -> Array GraphicsParam
background c = [PBackground c]
