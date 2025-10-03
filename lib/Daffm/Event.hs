module Daffm.Event where

import Brick (suspendAndResume')
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import Control.Monad.State (MonadIO (liftIO), MonadState, get, gets, modify, put)
import Daffm.State (cacheDirPosition, loadDirInAppState)
import Daffm.Types (AppState (..), FileInfo (..), FileType (..), FocusTarget (FocusCmdline, FocusMain))
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Zipper as Z
import Data.Vector ((!?))
import qualified Graphics.Vty as V
import System.FilePath (takeDirectory)
import System.Process (callCommand, callProcess)

type AppEvent = T.EventM FocusTarget AppState

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

appEvent :: T.BrickEvent FocusTarget e -> AppEvent ()
appEvent brickevent@(T.VtyEvent event) = do
  focusTarget <- gets stateFocusTarget
  case (focusTarget, event) of
    (FocusMain, V.EvKey (V.KChar 'l') []) -> openSelectedFile
    (FocusMain, V.EvKey (V.KChar 'h') []) -> goBackToParentDir
    (FocusMain, V.EvKey V.KEnter []) -> openSelectedFile
    (FocusMain, V.EvKey V.KBS []) -> goBackToParentDir
    (FocusMain, V.EvKey (V.KChar ':') []) -> modify (\st -> st {stateFocusTarget = FocusCmdline})
    (FocusMain, V.EvKey (V.KChar 'q') []) -> M.halt
    (FocusMain, V.EvKey (V.KChar 'r') [V.MCtrl]) -> reloadDir
    (FocusMain, _) -> do
      files <- gets stateFiles
      newFiles <- T.nestEventM' files (L.handleListEventVi L.handleListEvent event)
      modify (\appState -> appState {stateFiles = newFiles})
    (FocusCmdline, V.EvKey V.KEsc []) -> modify (\st -> st {stateFocusTarget = FocusMain})
    (FocusCmdline, V.EvKey V.KEnter []) -> do
      gets (trimCmd . Editor.getEditContents . stateCmdlineEditor) >>= evaluateCommand
      clearEditor
      modify (\st -> st {stateFocusTarget = FocusMain})
      where
        trimCmd = dropWhile isSpace . dropWhileEnd isSpace . unlines
    (FocusCmdline, _) -> do
      editor <- gets stateCmdlineEditor
      newEditor <- T.nestEventM' editor (Editor.handleEditorEvent brickevent)
      modify (\appState -> appState {stateCmdlineEditor = newEditor})
  modify cacheDirPosition
appEvent _ = pure ()

cmdSubstitutions :: Text.Text -> AppEvent Text.Text
cmdSubstitutions cmd = do
  file <- gets (maybe "" (filePath . snd) . L.listSelectedElement . stateFiles)
  pure $ Text.replace "%" (Text.pack file) cmd

evaluateCommand :: String -> AppEvent ()
evaluateCommand ('!' : cmd) = do
  cmd' <- Text.unpack <$> cmdSubstitutions (Text.pack cmd)
  suspendAndResume' $ callCommand cmd'
  reloadDir
evaluateCommand cmd = pure ()

clearEditor :: AppEvent ()
clearEditor = do
  editor <- gets stateCmdlineEditor
  let editor' = Editor.applyEdit Z.clearZipper editor
  modify (\s -> s {stateCmdlineEditor = editor'})

reloadDir :: AppEvent ()
reloadDir = do
  AppState {stateParentDir, stateCwd} <- get
  modifyM (liftIO . loadDirInAppState stateCwd stateParentDir)

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
