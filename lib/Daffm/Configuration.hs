module Daffm.Configuration where

import Control.Arrow (ArrowChoice (left))
import Control.Exception (throwIO)
import Daffm.Action.Commands (parseCommand)
import Daffm.Keymap (parseKeySequence)
import Daffm.Types
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Toml ((.=))
import qualified Toml

loadConfigFile :: IO Configuration
loadConfigFile = Text.readFile "./config.toml" >>= parse
  where
    parse txt = case parseConfig txt of
      Left e -> throwIO $ userError $ show e
      Right c -> pure c

parseConfig :: Text.Text -> Either Text.Text Configuration
parseConfig = left Toml.prettyTomlDecodeErrors . Toml.decode configurationCodec

configurationCodec :: Toml.TomlCodec Configuration
configurationCodec =
  Configuration
    <$> (keymapCodec "keymap" .= configKeymap)
    <*> (openerCodec "opener" .= configOpener)
    <*> pure Map.empty .= configTheme
  where
    openerCodec = Toml.dioptional . Toml.text

keymapCodec :: Toml.Key -> Toml.TomlCodec Keymap
keymapCodec = Toml.dimap (const Map.empty) toKeymap . keymapRawCodec
  where
    keymapRawCodec = Toml.tableMap Toml._KeyText Toml.text
    toKeymap = Map.fromList . map (bimap toKeys toCmd) . Map.toList
    toKeys = fromMaybe [] . parseKeySequence . stripQuotes
    toCmd = fromMaybe CmdNoop . parseCommand
    stripQuotes txt = fromMaybe txt (Text.stripPrefix "\"" txt >>= Text.stripSuffix "\"")
