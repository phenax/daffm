module Daffm.Event where

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import Control.Monad.State (gets, modify)
import Daffm.Action.Cmdline
import Daffm.Action.Core
import Daffm.State (cacheDirPosition)
import Daffm.Types (AppEvent, AppState (..), FocusTarget (..))
import qualified Graphics.Vty as V

appEvent :: T.BrickEvent FocusTarget e -> AppEvent ()
appEvent brickevent@(T.VtyEvent event) = do
  focusTarget <- gets stateFocusTarget
  case (focusTarget, event) of
    (FocusMain, V.EvKey (V.KChar 'l') []) -> openSelectedFile
    (FocusMain, V.EvKey (V.KChar 'h') []) -> goBackToParentDir
    (FocusMain, V.EvKey V.KEnter []) -> openSelectedFile
    (FocusMain, V.EvKey V.KBS []) -> goBackToParentDir
    (FocusMain, V.EvKey (V.KChar '~') []) -> goHome
    (FocusMain, V.EvKey (V.KChar ':') []) -> enterCmdline
    (FocusMain, V.EvKey (V.KChar '!') []) -> setCmdlineText "!" >> enterCmdline
    (FocusMain, V.EvKey (V.KChar 'q') []) -> M.halt
    (FocusMain, V.EvKey (V.KChar 'r') [V.MCtrl]) -> reloadDir
    (FocusMain, V.EvKey (V.KChar 'v') []) -> toggleCurrentFileSelection
    (FocusMain, V.EvKey (V.KChar 'C') []) -> clearFileSelections
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
