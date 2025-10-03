{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use for_" #-}
module Daffm.Action.Core where

import Brick (suspendAndResume')
import qualified Brick.Widgets.List as L
import Control.Monad.State (MonadIO (liftIO), MonadState, get, gets, put)
import Daffm.State (loadDirInAppState)
import Daffm.Types (AppEvent, AppState (..), FileInfo (..), FileType (..))
import Data.Vector ((!?))
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

openSelectedFile :: AppEvent ()
openSelectedFile = do
  appState <- get
  let indexM = L.listSelected $ stateFiles appState
  let files = L.listElements $ stateFiles appState
  case indexM >>= (files !?) of
    Just file -> openFile appState file
    Nothing -> pure ()
  pure ()

openFile :: AppState -> FileInfo -> AppEvent ()
openFile appState (FileInfo {filePath, fileType = Directory}) = do
  modifyM (liftIO . loadDirInAppState filePath (stateCwd appState))
openFile _appState (FileInfo {filePath, fileType}) = do
  suspendAndResume' $ do
    putStrLn $ "Opening " <> show fileType <> ": " <> filePath
    callProcess "nvim" [filePath]
  pure ()
