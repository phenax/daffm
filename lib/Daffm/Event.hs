module Daffm.Event where

import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import Control.Monad.State (gets, modify)
import Daffm.Action.Cmdline
import Daffm.Action.Commands
import Daffm.Action.Keymap (appendToKeySequence, processKeySequence)
import Daffm.State (cacheDirPosition)
import Daffm.Types (AppEvent, AppState (..), FocusTarget (..), KeyMatchResult (..))
import qualified Graphics.Vty as V

appEvent :: T.BrickEvent FocusTarget e -> AppEvent ()
appEvent brickevent@(T.VtyEvent event) = do
  focusTarget <- gets stateFocusTarget
  case (focusTarget, event) of
    (FocusCmdline, V.EvKey V.KEsc []) -> leaveCmdline
    (FocusCmdline, V.EvKey V.KEnter []) -> runCmdline
    (FocusCmdline, _) -> do
      editor <- gets stateCmdlineEditor
      newEditor <- T.nestEventM' editor (Editor.handleEditorEvent brickevent)
      modify (\appState -> appState {stateCmdlineEditor = newEditor})
    (FocusMain, V.EvKey key []) -> do
      appendToKeySequence key
      processKeySequence >>= \case
        MatchFailure -> do
          files <- gets stateFiles
          newFiles <- T.nestEventM' files (L.handleListEventVi L.handleListEvent event)
          modify (\appState -> appState {stateFiles = newFiles})
        _ -> pure ()
    (FocusMain, _) -> do
      files <- gets stateFiles
      newFiles <- T.nestEventM' files (L.handleListEventVi L.handleListEvent event)
      modify (\appState -> appState {stateFiles = newFiles})
  modify cacheDirPosition
appEvent _ = pure ()
