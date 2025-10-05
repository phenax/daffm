module Daffm.Action.Keymap where

import Control.Monad.State (get, modify)
import Daffm.Action.Commands
import Daffm.Keymap (matchKeySequence)
import Daffm.Types

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
