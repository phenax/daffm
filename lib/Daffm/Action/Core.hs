{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use for_" #-}
{-# HLINT ignore "Use <=<" #-}
module Daffm.Action.Core where

import Brick (suspendAndResume')
import qualified Brick.Widgets.List as L
import Control.Monad.State (MonadIO (liftIO), MonadState, get, gets, modify, put)
import Daffm.State
import Daffm.Types (AppEvent, AppState (..), FileInfo (..), FilePathText, FileType (..))
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Directory (getHomeDirectory)
import System.FilePath (takeDirectory)
import System.Process (callProcess)

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

loadDir :: FilePathText -> FilePathText -> AppEvent ()
loadDir dir parentDir = do
  modifyM (liftIO . (>>= filterInvalidSelections) . loadDirToState dir parentDir)

reloadDir :: AppEvent ()
reloadDir = do
  AppState {stateCwd, stateParentDir} <- get
  loadDir stateCwd stateParentDir

goBackToParentDir :: AppEvent ()
goBackToParentDir = do
  dir <- gets stateParentDir
  loadDir dir (Text.pack . takeDirectory $ Text.unpack dir)

changeDir :: FilePathText -> AppEvent ()
changeDir dir = do
  loadDir dir (Text.pack $ takeDirectory $ Text.unpack dir)

goHome :: AppEvent ()
goHome = do
  liftIO getHomeDirectory >>= changeDir . Text.pack

openSelectedFile :: AppEvent ()
openSelectedFile = do
  currentFile >>= \case
    Just file -> openFile file
    Nothing -> pure ()

openFile :: FileInfo -> AppEvent ()
openFile (FileInfo {filePath, fileType = Directory}) = do
  gets stateCwd >>= loadDir filePath
openFile (FileInfo {filePath, fileType}) = do
  suspendAndResume' $ do
    putStrLn $ "Opening " <> show fileType <> ": " <> Text.unpack filePath
    callProcess "nvim" [Text.unpack filePath]

currentFile :: AppEvent (Maybe FileInfo)
currentFile = do
  gets (fmap snd . L.listSelectedElement . stateFiles)

toggleCurrentFileSelection :: AppEvent ()
toggleCurrentFileSelection = do
  currentFile >>= \case
    Just file -> modify $ toggleFileSelection (filePath file)
    Nothing -> pure ()
  moveCurrent 1

clearFileSelections :: AppEvent ()
clearFileSelections =
  modify $ \s -> s {stateFileSelections = Set.empty}

moveCurrent :: Int -> AppEvent ()
moveCurrent count = do
  files <- gets stateFiles
  modify $ \s -> s {stateFiles = L.listMoveBy count files}
