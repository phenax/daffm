{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use for_" #-}
module Daffm.Action.Commands where

import qualified Brick as M
import Control.Monad (forM_)
import Daffm.Action.Cmdline
import Daffm.Action.Core
import Daffm.Types
import Daffm.Utils (trimStart)
import Data.Bifunctor (Bifunctor (second))
import Data.Char (isSpace)
import qualified Data.Text as Text

runCmdline :: AppEvent ()
runCmdline = do
  cmd <- getCmdlineText
  leaveCmdline
  evaluateCommand cmd

parseCommand :: Text.Text -> Maybe Command
parseCommand (Text.splitAt 2 -> ("!!", cmd)) = Just $ CmdShell True cmd
parseCommand (Text.splitAt 1 -> ("!", cmd)) = Just $ CmdShell False cmd
parseCommand cmd = mkCmd . splitCmdArgs $ trimStart cmd
  where
    splitCmdArgs = second trimStart . Text.break isSpace
    mkCmd = \case
      ("q", _) -> Just CmdQuit
      ("quit", _) -> Just CmdQuit
      ("shell!", cmd') -> Just $ CmdShell True cmd'
      ("shell", cmd') -> Just $ CmdShell False cmd'
      ("back", _) -> Just CmdGoBack
      ("open", _) -> Just CmdOpenSelection
      ("reload", _) -> Just CmdReload
      ("cd", dir) -> Just $ CmdChangeDir dir
      ("noop", _) -> Just CmdNoop
      ("cmdline-enter", _) -> Just CmdEnterCmdline
      ("cmdline-leave", _) -> Just CmdLeaveCmdline
      ("cmdline-set", txt) -> Just $ CmdSetCmdline txt
      ("selection-toggle", _) -> Just CmdToggleSelection
      ("selection-clear", _) -> Just CmdClearSelection
      _ -> Nothing

processCommand :: Command -> AppEvent ()
processCommand (CmdShell waitForKey cmd) = do
  cmdSubstitutions cmd >>= suspendAndRunShellCommand waitForKey
  reloadDir
processCommand CmdQuit = M.halt
processCommand (CmdSetCmdline txt) = enterCmdline >> setCmdlineText txt
processCommand CmdEnterCmdline = enterCmdline
processCommand CmdLeaveCmdline = leaveCmdline
processCommand CmdOpenSelection = openSelectedFile
processCommand (CmdChangeDir dir) = changeDir dir
processCommand CmdReload = reloadDir
processCommand CmdToggleSelection = toggleCurrentFileSelection
processCommand CmdClearSelection = clearFileSelections
processCommand CmdGoBack = goBackToParentDir
processCommand (CmdChain chain) = forM_ chain processCommand
processCommand CmdNoop = pure ()

evaluateCommand :: Text.Text -> AppEvent ()
evaluateCommand cmdtxt =
  case parseCommand cmdtxt of
    Just cmd -> processCommand cmd
    Nothing -> pure ()
