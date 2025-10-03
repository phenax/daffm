module Daffm.Event where

import Brick (suspendAndResume')
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import Control.Monad.State (MonadIO (liftIO), MonadState, get, gets, modify, put)
import Daffm.State (cacheDirPosition, loadDirInAppState)
import Daffm.Types (AppState (..), FileInfo (..), FileType (..), FocusTarget (FocusCmdline, FocusMain))
import Data.Vector ((!?))
import qualified Graphics.Vty as V
import System.FilePath (takeDirectory)
import System.Process (callProcess)

type AppEvent = T.EventM FocusTarget AppState

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

appEvent :: T.BrickEvent FocusTarget e -> AppEvent ()
appEvent brickevent@(T.VtyEvent event) = do
  focusTarget <- gets stateFocusTarget
  case (focusTarget, event) of
    (FocusCmdline, V.EvKey V.KEsc []) -> modify (\st -> st {stateFocusTarget = FocusMain})
    (FocusMain, V.EvKey (V.KChar ':') []) -> modify (\st -> st {stateFocusTarget = FocusCmdline})
    (FocusMain, V.EvKey (V.KChar 'q') []) -> M.halt
    (FocusMain, V.EvKey (V.KChar 'l') []) -> openSelectedFile
    (FocusMain, V.EvKey (V.KChar 'h') []) -> goBackToParentDir
    (FocusMain, V.EvKey V.KEnter []) -> openSelectedFile
    (FocusMain, V.EvKey V.KBS []) -> goBackToParentDir
    (FocusMain, _) -> do
      files <- gets stateFiles
      newFiles <- T.nestEventM' files (L.handleListEventVi L.handleListEvent event)
      modify (\appState -> appState {stateFiles = newFiles})
    (FocusCmdline, _) -> do
      editor <- gets stateCmdlineEditor
      newEditor <- T.nestEventM' editor (Editor.handleEditorEvent brickevent)
      modify (\appState -> appState {stateCmdlineEditor = newEditor})
  modify cacheDirPosition
appEvent _ = pure ()

openSelectedFile :: AppEvent ()
openSelectedFile = do
  AppState {stateFiles, stateCwd} <- get
  let indexM = L.listSelected stateFiles
  let files = L.listElements stateFiles
  case indexM >>= (files !?) of
    Just (FileInfo {filePath, fileType = Directory}) ->
      modifyM (liftIO . loadDirInAppState filePath stateCwd)
    Just (FileInfo {filePath, fileType}) -> do
      suspendAndResume' $ do
        putStrLn $ "Opening " <> show fileType <> ": " <> filePath
        callProcess "nvim" [filePath]
      pure ()
    Nothing -> pure ()
  pure ()

goBackToParentDir :: AppEvent ()
goBackToParentDir = do
  dir <- gets stateParentDir
  modifyM (liftIO . loadDirInAppState dir (takeDirectory dir))
