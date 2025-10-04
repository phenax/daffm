module Daffm.Configuration where

import Control.Arrow (ArrowChoice (left))
import Control.Exception (throwIO)
import Daffm.Action.Commands (parseCommand)
import Daffm.Types
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Graphics.Vty as V
import Toml ((.=))
import qualified Toml

loadConfigFile :: IO Configuration
loadConfigFile = Text.readFile "./config.toml" >>= parse
  where
    parse txt = case parseConfig txt of
      Left e -> throwIO $ userError $ show e
      Right c -> pure c

parseConfig :: Text.Text -> Either Text.Text Configuration
parseConfig txt = left Toml.prettyTomlDecodeErrors $ Toml.decode configurationCodec txt

configurationCodec :: Toml.TomlCodec Configuration
configurationCodec =
  Configuration
    <$> (keymapCodec "keymap" .= configKeymap)
    <*> pure Map.empty .= configTheme
  where
    keymapCodec = Toml.dimap (const Map.empty) toKeymap . keymapRawCodec
    keymapRawCodec = Toml.tableMap Toml._KeyText Toml.text
    toKeymap = Map.fromList . map (bimap toKeys toCmd) . Map.toList
    toKeys = fromMaybe [] . parseKey
    toCmd = fromMaybe CmdNoop . parseCommand

parseKey :: Text.Text -> Maybe [Key]
parseKey keytxt = parse keytxt []
  where
    -- TODO: Refactor using https://hackage-content.haskell.org/package/brick-2.9/docs/Brick-Keybindings-Parse.html#v:parseBinding
    parse k keys = case k of
      (Text.null -> True) -> pure keys
      (Text.splitAt 1 -> (c, rest)) -> case (c, rest) of
        ("<", Text.splitAt 4 -> (Text.toLower -> "tab>", rest')) -> parse rest' $ keys <> [V.KChar '\t']
        ("<", Text.splitAt 3 -> (Text.toLower -> "cr>", rest')) -> parse rest' $ keys <> [V.KEnter]
        ("<", Text.splitAt 4 -> (Text.toLower -> "esc>", rest')) -> parse rest' $ keys <> [V.KEsc]
        _ -> do
          ch <- fst <$> Text.uncons c
          parse rest $ keys <> [V.KChar ch]
