module Daffm.Args where

import Control.Exception (throwIO)
import Daffm.Types
import qualified Data.Text as Text

parseArgs :: [String] -> IO Args
parseArgs rawArgs = case parsedArgs of
  Left e -> throwIO $ userError e
  Right v -> pure v
  where
    parsedArgs = parse rawArgs (Args {argsDirOrFile = Nothing, argsConfigFile = Nothing, argsHelp = False})
    parse :: [String] -> Args -> Either String Args
    parse [] args = Right args
    parse ("-h" : _) args = Right $ args {argsHelp = True}
    parse ("--help" : _) args = Right $ args {argsHelp = True}
    parse ["-c"] _ = Left "Missing value for -c arg"
    parse ("-c" : config : rest) args = parse rest $ args {argsConfigFile = Just config}
    parse ["--config"] _ = Left "Missing value for --config arg"
    parse ("--config" : config : rest) args = parse rest $ args {argsConfigFile = Just config}
    parse (flag@('-' : _) : _) _ = Left $ "Invalid flag " <> flag
    parse (dir : rest) args = parse rest $ args {argsDirOrFile = Just $ Text.pack dir}

helpMenuContents :: String
helpMenuContents =
  unlines
    [ "daffm - Dumb as-fuck file manager",
      "",
      "Usage: daffm [options] [dir]",
      "",
      "Arguments:",
      "  [dir]",
      "      Directory or file path to load. Defaults to current working directory",
      "",
      "Options:",
      "  -c, --config <CONFIG-PATH>",
      "      Load toml config from file",
      "      If path is prefixed with @, will use alternate config",
      "        Ex: -c @foo will load $XDG_CONFIG_HOME/daffm/config.foo.toml",
      "      Default: $XDG_CONFIG_HOME/daffm/config.toml",
      "",
      "  -h, --help",
      "      This help menu"
    ]
