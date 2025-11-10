{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <=<" #-}
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
import qualified Graphics.Vty as K
import System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)
import System.FilePath (joinPath)
import Toml ((.=))
import qualified Toml

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    { configKeymap = defaultKeymaps,
      configOpener = Nothing,
      configCommands = Map.empty,
      configExtend = Nothing
    }

defaultKeymaps :: Keymap
defaultKeymaps =
  Map.fromList
    [ ([K.KChar 'q'], CmdQuit),
      ([K.KChar 'r', K.KChar 'r'], CmdReload),
      ([K.KChar '!'], CmdSetCmdline "!"),
      ([K.KChar '/'], CmdSetCmdline "/"),
      ([K.KChar 'n'], CmdSearchNext 1),
      ([K.KChar 'N'], CmdSearchNext (-1)),
      ([K.KChar ':'], CmdEnterCmdline),
      ([K.KChar 'l'], CmdSelectionOpen),
      ([K.KChar 'h'], CmdGoBack),
      ([K.KEnter], CmdSelectionOpen),
      ([K.KBS], CmdGoBack),
      ([K.KChar 'v'], CmdSelectionToggle),
      ([K.KChar '\t'], CmdSelectionToggle),
      ([K.KChar 'C'], CmdSelectionClear),
      ([K.KChar '~'], CmdChangeDir "~"),
      ([K.KChar '$'], CmdShell False "$SHELL"),
      ([K.KChar 'g', K.KChar 'x'], CmdShell False "!xdg-open % >/dev/null 2>&1"),
      ([K.KChar 'g', K.KChar 'h'], CmdChangeDir "~"),
      ([K.KChar 'g', K.KChar 'c', K.KChar 'f', K.KChar 'g'], CmdChangeDir "~/.config/daffm"),
      ([K.KChar 'g', K.KChar 'g'], CmdMove $ MoveTo 0),
      ([K.KChar 'g', K.KChar 'k'], CmdMove $ MoveTo 0),
      ([K.KChar 'g', K.KChar 'j'], CmdMove MoveToEnd),
      ([K.KChar 'G'], CmdMove MoveToEnd)
    ]

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
  cfgPath <- resolveConfigPath pathM
  config <- load cfgPath
  case configExtend config of
    Just path -> do
      baseCfg <- loadConfigFile $ Just (Text.unpack path)
      pure $ config <> baseCfg
    _ -> pure config
  where
    load = (>>= parseWithDefault) . IO.try . Text.readFile
    parseWithDefault :: Either IOError Text.Text -> IO Configuration
    parseWithDefault rawE = case rawE of
      Left _ -> pure defaultConfiguration
      Right txt -> (<> defaultConfiguration) <$> parse txt
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
    <*> (extendCodec "extend" .= configExtend)
    <*> (commandsCodec "commands" .= configCommands)
  where
    openerCodec = Toml.dioptional . Toml.text
    extendCodec = Toml.dioptional . Toml.text

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

commandsCodec :: Toml.Key -> Toml.TomlCodec (Map.Map Text.Text Command)
commandsCodec = Toml.tableMap Toml._KeyText commandCodec
  where
    toCmd = fromMaybe CmdNoop . parseCommand
    commandCodec k = cmdCodec k <|> cmdChainCodec k
    cmdCodec = Toml.dimap (const "") toCmd . Toml.text
    cmdChainCodec = Toml.dimap (const []) (CmdChain . map toCmd) . Toml.arrayOf Toml._Text
