module Main where

import qualified Brick.Main as M
import Control.Monad (void)
import qualified Daffm
import qualified Data.Text as Text
import System.Directory (getCurrentDirectory)
import System.FilePath (takeDirectory)

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let parentDir = Text.pack $ takeDirectory cwd
  initialState <- Daffm.loadDirToState (Text.pack cwd) parentDir Daffm.mkEmptyAppState
  void $ M.defaultMain Daffm.app initialState
