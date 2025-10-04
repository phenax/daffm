module Daffm.Event where

import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import Control.Monad.State (get, gets, modify)
import Daffm.Action.Cmdline
import Daffm.Action.Commands
import Daffm.State (cacheDirPosition)
import Daffm.Types (AppEvent, AppState (..), Command (..), FocusTarget (..), Key, KeyMatchResult (..), KeySequence, Keymap)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
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

matchKeySequence :: Keymap -> KeySequence -> KeyMatchResult
matchKeySequence keymaps keys
  | Map.member keys keymaps =
      MatchSuccess . fromMaybe CmdNoop $ Map.lookup keys keymaps
  | otherwise = partial keymaps keys
  where
    partial _ [] = MatchFailure
    partial (Map.null -> True) _ = MatchFailure
    partial keymaps' keys' = if hasMatch then MatchPartial else MatchFailure
      where
        hasMatch = any (startsWith keys' . fst) (Map.toList keymaps')
    startsWith ls1 ls2 = ls1 == take (length ls1) ls2

processKeySequence :: AppEvent KeyMatchResult
processKeySequence = do
  (AppState {stateKeyMap, stateKeySequence}) <- get
  let match = matchKeySequence stateKeyMap stateKeySequence
  case match of
    MatchSuccess cmd -> do
      processCommand cmd
      modify (\st -> st {stateKeySequence = []})
    MatchPartial -> pure ()
    MatchFailure -> do
      modify (\st -> st {stateKeySequence = []})
  pure match

appendToKeySequence :: Key -> AppEvent ()
appendToKeySequence key =
  modify (\st -> st {stateKeySequence = stateKeySequence st <> [key]})
