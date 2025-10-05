module Daffm.Keymap where

import Daffm.Types
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Graphics.Vty as V

showKeySequence :: KeySequence -> Text.Text
showKeySequence = Text.intercalate "" . map showKey

-- TODO: More chars
showKey :: Key -> Text.Text
showKey (V.KChar ' ') = "<space>"
showKey (V.KChar '\t') = "<tab>"
showKey (V.KChar c) = Text.singleton c
showKey V.KEnter = "<cr>"
showKey V.KEsc = "<esc>"
showKey V.KBS = "<bs>"
showKey _ = ""

matchKeySequence :: Keymap -> KeySequence -> KeyMatchResult
matchKeySequence keymaps keys
  | Map.member keys keymaps =
      MatchSuccess . fromMaybe CmdNoop $ Map.lookup keys keymaps
  | otherwise = partial keymaps keys
  where
    partial _ [] = MatchFailure
    partial (Map.null -> True) _ = MatchFailure
    partial keymaps' keys' = if hasMatch then MatchPartial else MatchFailure
      where
        hasMatch = any (startsWith keys' . fst) (Map.toList keymaps')
    startsWith ls1 ls2 = ls1 == take (length ls1) ls2

parseKeySequence :: Text.Text -> Maybe [Key]
parseKeySequence keytxt = parse keytxt []
  where
    parse k keys = case k of
      "" -> pure keys
      (Text.stripPrefix "<tab>" -> (Just rest')) -> parse rest' $ keys <> [V.KChar '\t']
      (Text.stripPrefix "<space>" -> (Just rest')) -> parse rest' $ keys <> [V.KChar ' ']
      (Text.stripPrefix "<bs>" -> (Just rest')) -> parse rest' $ keys <> [V.KBS]
      (Text.stripPrefix "<cr>" -> (Just rest')) -> parse rest' $ keys <> [V.KEnter]
      (Text.stripPrefix "<esc>" -> (Just rest')) -> parse rest' $ keys <> [V.KEsc]
      (Text.splitAt 1 -> (c, rest)) -> do
        ch <- fst <$> Text.uncons c
        parse rest $ keys <> [V.KChar ch]
