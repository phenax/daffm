module Daffm.Event where

import Brick (suspendAndResume')
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import Control.Monad (void)
import Control.Monad.State (MonadIO (liftIO), MonadState, get, gets, modify, put)
import Daffm.State (cacheDirPosition, loadDirInAppState)
import Daffm.Types (AppEvent, AppState (..), FileInfo (..), FileType (..), FocusTarget (..))
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Text as Text
import qualified Data.Text.Zipper as Z
import qualified Data.Text.Zipper as Zipper
import Data.Vector ((!?))
import qualified Graphics.Vty as V
import System.FilePath (takeDirectory)
import System.Process (callCommand, callProcess)

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
    (FocusMain, V.EvKey (V.KChar ':') []) -> enterCmdline
    (FocusMain, V.EvKey (V.KChar '!') []) -> setCmdlineText "!" >> enterCmdline
    (FocusMain, V.EvKey (V.KChar 'q') []) -> M.halt
    (FocusMain, V.EvKey (V.KChar 'r') [V.MCtrl]) -> reloadDir
    -- Just for testing
    (FocusMain, V.EvKey (V.KChar 'p') [V.MCtrl]) -> evaluateCommand "!!chafa -f kitty %"
    (FocusCmdline, V.EvKey V.KEsc []) -> leaveCmdline
    (FocusCmdline, V.EvKey V.KEnter []) -> runCmdline
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

leaveCmdline :: AppEvent ()
leaveCmdline = clearCmdline >> modify (\st -> st {stateFocusTarget = FocusMain})

enterCmdline :: AppEvent ()
enterCmdline = modify (\st -> st {stateFocusTarget = FocusCmdline})

setCmdlineText :: String -> AppEvent ()
setCmdlineText text =
  applyCmdlineEdit (const $ Z.stringZipper [text] (Just 1))

clearCmdline :: AppEvent ()
clearCmdline = applyCmdlineEdit Z.clearZipper

cmdSubstitutions :: Text.Text -> AppEvent Text.Text
cmdSubstitutions cmd = do
  (AppState {stateFiles, stateCwd}) <- get
  let file = maybe "" (filePath . snd) . L.listSelectedElement $ stateFiles
  -- TODO: Escaping %
  let subst =
        Text.replace "%" (Text.pack file)
          . Text.replace "%d" (Text.pack stateCwd)
  pure . subst $ cmd

runCmdline :: AppEvent ()
runCmdline = do
  cmd <- gets (trimCmd . Editor.getEditContents . stateCmdlineEditor)
  evaluateCommand cmd
  leaveCmdline
  where
    trimCmd = dropWhile isSpace . dropWhileEnd isSpace . unlines

evaluateCommand :: String -> AppEvent ()
evaluateCommand ('!' : '!' : cmd) = do
  cmd' <- Text.unpack <$> cmdSubstitutions (Text.pack cmd)
  suspendAndResume' $ do
    callCommand cmd'
    putStrLn "Press any key to continue"
    void getChar
  reloadDir
evaluateCommand ('!' : cmd) = do
  cmd' <- Text.unpack <$> cmdSubstitutions (Text.pack cmd)
  suspendAndResume' $ callCommand cmd'
  reloadDir
evaluateCommand _cmd = pure ()

reloadDir :: AppEvent ()
reloadDir = do
  AppState {stateParentDir, stateCwd} <- get
  modifyM (liftIO . loadDirInAppState stateCwd stateParentDir)

applyCmdlineEdit :: (Zipper.TextZipper String -> Zipper.TextZipper String) -> AppEvent ()
applyCmdlineEdit zipper = do
  editor <- gets stateCmdlineEditor
  let editor' = Editor.applyEdit zipper editor
  modify (\s -> s {stateCmdlineEditor = editor'})

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
