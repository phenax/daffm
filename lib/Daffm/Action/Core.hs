{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use for_" #-}
module Daffm.Action.Core where

import Brick (suspendAndResume')
import qualified Brick.Widgets.List as L
import Control.Monad.State (MonadIO (liftIO), MonadState, get, gets, modify, put)
import Daffm.State (loadDirInAppState, toggleFileSelection)
import Daffm.Types (AppEvent, AppState (..), FileInfo (..), FileType (..))
import qualified Data.Set as Set
import System.Directory (getHomeDirectory)
import System.FilePath (takeDirectory)
import System.Process (callProcess)

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

reloadDir :: AppEvent ()
reloadDir = do
  AppState {stateCwd, stateParentDir} <- get
  modifyM (liftIO . loadDirInAppState stateCwd stateParentDir)

goBackToParentDir :: AppEvent ()
goBackToParentDir = do
  dir <- gets stateParentDir
  modifyM (liftIO . loadDirInAppState dir (takeDirectory dir))

goHome :: AppEvent ()
goHome = do
  dir <- liftIO getHomeDirectory
  modifyM (liftIO . loadDirInAppState dir (takeDirectory dir))

openSelectedFile :: AppEvent ()
openSelectedFile = do
  fileM <- currentFile
  case fileM of
    Just file -> openFile file
    Nothing -> pure ()

openFile :: FileInfo -> AppEvent ()
openFile (FileInfo {filePath, fileType = Directory}) = do
  (AppState {stateCwd}) <- get
  modifyM (liftIO . loadDirInAppState filePath stateCwd)
openFile (FileInfo {filePath, fileType}) = do
  suspendAndResume' $ do
    putStrLn $ "Opening " <> show fileType <> ": " <> filePath
    callProcess "nvim" [filePath]

currentFile :: AppEvent (Maybe FileInfo)
currentFile = do
  gets (fmap snd . L.listSelectedElement . stateFiles)

toggleCurrentFileSelection :: AppEvent ()
toggleCurrentFileSelection = do
  fileM <- currentFile
  case fileM of
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
