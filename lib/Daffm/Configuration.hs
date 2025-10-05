module Daffm.Configuration where

import Control.Applicative ((<|>))
import Control.Arrow (ArrowChoice (left))
import Control.Exception (throwIO)
import qualified Control.Exception as IO
import Daffm.Action.Commands (parseCommand)
import Daffm.Keymap (parseKeySequence)
import Daffm.Types
import Data.Bifunctor (Bifunctor (first))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)
import System.FilePath (joinPath)
import Toml ((.=))
import qualified Toml

getConfigDir :: IO FilePath
getConfigDir = getXdgDirectory XdgConfig "daffm"

getDefaultConfigFilePath :: String -> IO FilePath
getDefaultConfigFilePath "" = do
  dir <- getConfigDir
  pure $ joinPath [dir, "config.toml"]
getDefaultConfigFilePath name = do
  dir <- getConfigDir
  pure $ joinPath [dir, "config." <> name <> ".toml"]

resolveConfigPath :: Maybe String -> IO FilePath
resolveConfigPath Nothing = getDefaultConfigFilePath ""
resolveConfigPath (Just ('@' : name)) = getDefaultConfigFilePath name
resolveConfigPath (Just path) = pure path

loadConfigFile :: Maybe String -> IO Configuration
loadConfigFile pathM = do
  resolveConfigPath pathM >>= (IO.try . Text.readFile) >>= foobar
  where
    foobar :: Either IOError Text.Text -> IO Configuration
    foobar rawE = case rawE of
      Left _ -> pure defaultConfiguration
      Right txt -> parse txt
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
    keymapRawCodec = Toml.tableMap Toml._KeyText commandCodec
    toKeymap = Map.fromList . map (first toKeys) . Map.toList
    toKeys = fromMaybe [] . parseKeySequence . stripQuotes
    toCmd = fromMaybe CmdNoop . parseCommand
    stripQuotes txt = fromMaybe txt (Text.stripPrefix "\"" txt >>= Text.stripSuffix "\"")
    commandCodec k = cmdCodec k <|> cmdChainCodec k
    cmdCodec = Toml.dimap (const "") toCmd . Toml.text
    cmdChainCodec = Toml.dimap (const []) (CmdChain . map toCmd) . Toml.arrayOf Toml._Text
