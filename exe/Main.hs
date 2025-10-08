module Main where

import qualified Daffm
import qualified Daffm.Args as Args
import Daffm.Configuration (loadConfigFile)
import Daffm.Types
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= Args.parseArgs >>= evaluateArgs

evaluateArgs :: Args -> IO ()
evaluateArgs (Args {argsHelp = True}) = putStrLn Args.helpMenuContents
evaluateArgs (Args {argsDirOrFile, argsConfigFile}) = do
  cwd <- getCurrentDirectory
  config <- loadConfigFile argsConfigFile
  let dir = fromMaybe (Text.pack cwd) argsDirOrFile
  Daffm.initApp dir config
