module Daffm.Utils where

import Data.Char (isSpace)
import qualified Data.Text as Text

trimStart :: Text.Text -> Text.Text
trimStart = Text.dropWhile isSpace

trimEnd :: Text.Text -> Text.Text
trimEnd = Text.dropWhileEnd isSpace

trim :: Text.Text -> Text.Text
trim = trimStart . trimEnd
