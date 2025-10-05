module Main where

import qualified Brick.Main as M
import Control.Monad (void)
import qualified Daffm
import Daffm.Configuration (loadConfigFile)
import qualified Data.Text as Text
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  config <- loadConfigFile
  initialState <- Daffm.loadDirToState (Text.pack cwd) $ Daffm.mkEmptyAppState config
  void $ M.defaultMain Daffm.app initialState
