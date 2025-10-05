{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use for_" #-}
module Daffm.Action.Commands where

import Brick (suspendAndResume')
import qualified Brick as M
import qualified Brick.Widgets.List as L
import Control.Monad (void)
import Control.Monad.State (get)
import Daffm.Action.Cmdline
import Daffm.Action.Core
import Daffm.Types
import Daffm.Utils (trimStart)
import Data.Bifunctor (Bifunctor (second))
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified GHC.IO.Exception as Proc
import System.Exit (ExitCode)
import qualified System.Process as Proc

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
    splitCmdArgs = second trimStart . Text.splitAt cmdEndIdx
    cmdEndIdx = fromMaybe (Text.length cmd) $ Text.findIndex isSpace cmd
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

-- Suspend tui and run shell command
-- When waitForKey is true, it will prompt for a key press on success
-- When exit code is non-zero, it will print it and prompt for key press regardless of waitForKey
suspendAndRunShellCommand :: Bool -> Text.Text -> AppEvent ()
suspendAndRunShellCommand waitForKey cmd = do
  suspendAndResume' $ do
    exitCode <- shellCommand $ Text.unpack cmd
    case exitCode of
      Proc.ExitFailure code -> do
        putStrLn $ "Process exited with " <> show code
        putStrLn "Press any key to continue" >> void getChar
      _ | waitForKey -> putStrLn "Press any key to continue" >> void getChar
      _ -> pure ()

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
processCommand CmdNoop = pure ()

shellCommand :: String -> IO ExitCode
shellCommand cmd = do
  Proc.withCreateProcess
    (Proc.shell cmd) {Proc.delegate_ctlc = True}
    $ \_ _ _ p -> Proc.waitForProcess p

cmdSubstitutions :: Text.Text -> AppEvent Text.Text
cmdSubstitutions cmd = do
  (AppState {stateFiles, stateCwd, stateFileSelections}) <- get
  let file = maybe "" (filePath . snd) . L.listSelectedElement $ stateFiles
  let escape = (\s -> "'" <> s <> "'") . Text.replace "'" "\\'"
  let selections = Set.elems stateFileSelections
  let selectionsOrCurrent = if Set.null stateFileSelections then [file] else selections
  let subst =
        Text.replace "%" file
          . Text.replace "%d" stateCwd
          . Text.replace "%s" (Text.unwords $ map escape selections)
          . Text.replace "%S" (Text.dropWhileEnd (== '\n') $ Text.unlines selections)
          . Text.replace "%f" (Text.unwords $ map escape selectionsOrCurrent)
          . Text.replace "%F" (Text.dropWhileEnd (== '\n') $ Text.unlines selectionsOrCurrent)
  pure . subst $ cmd

evaluateCommand :: Text.Text -> AppEvent ()
evaluateCommand cmdtxt =
  case parseCommand cmdtxt of
    Just cmd -> processCommand cmd
    Nothing -> pure ()
