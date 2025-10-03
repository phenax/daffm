module Main where

import qualified Brick.Main as M
import Control.Monad (void)
import qualified Daffm
import System.Directory (getCurrentDirectory)
import System.FilePath (takeDirectory)

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let parentDir = takeDirectory cwd
  initialState <- Daffm.loadDirToState cwd parentDir Daffm.mkEmptyAppState
  void $ M.defaultMain Daffm.app initialState
