{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use for_" #-}
module Daffm.Action.Commands where

import qualified Brick as M
import qualified Brick.Widgets.List as L
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (gets, modify)
import Daffm.Action.Cmdline
import Daffm.Action.Core
import Daffm.Keymap (parseKeySequence)
import Daffm.Types
import Daffm.Utils (trim, trimStart)
import Data.Bifunctor (Bifunctor (second))
import Data.Char (isSpace)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Process as Proc
import Text.Read (readMaybe)

runCmdline :: AppEvent ()
runCmdline = do
  cmd <- getCmdlineText
  leaveCmdline
  evaluateCommand cmd

parseCommand :: Text.Text -> Maybe Command
parseCommand (Text.stripPrefix "!!" -> Just cmd) = Just $ CmdShell True cmd
parseCommand (Text.stripPrefix "!" -> Just cmd) = Just $ CmdShell False cmd
parseCommand (Text.stripPrefix "/" -> Just term) = Just $ CmdSearch $ trim term
parseCommand cmd = mkCmd . splitCmdArgs $ trimStart cmd
  where
    splitCmdArgs = second trimStart . Text.break isSpace
    mkCmd = \case
      ("q", _) -> Just CmdQuit
      ("quit", _) -> Just CmdQuit
      ("shell!", cmd') -> Just $ CmdShell True cmd'
      ("shell", cmd') -> Just $ CmdShell False cmd'
      ("eval", cmd') -> Just $ CmdCommandShell cmd'
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
      ("search", term) -> Just $ CmdSearch $ trim term
      ("search-next", _) -> Just $ CmdSearchNext 1
      ("search-prev", _) -> Just $ CmdSearchNext (-1)
      ("map", Text.break isSpace -> (keysraw, cmdraw)) -> do
        keys <- parseKeySequence keysraw
        cmd' <- parseCommand $ trimStart cmdraw
        pure $ CmdKeymapSet keys cmd'
      ("move", Text.stripPrefix "$" -> Just _) -> Just $ CmdMove MoveToEnd
      ("move", Text.stripPrefix "+" -> Just inc) -> Just . CmdMove . MoveDown . read $ Text.unpack inc
      ("move", Text.stripPrefix "-" -> Just inc) -> Just . CmdMove . MoveUp . read $ Text.unpack inc
      ("move", readMaybe . Text.unpack -> Just pos) -> Just . CmdMove . MoveTo $ pos
      _ -> Nothing

readCommandLines' :: Text.Text -> IO [Text.Text]
readCommandLines' cmd = do
  Proc.withCreateProcess
    (Proc.shell $ Text.unpack cmd)
      { Proc.delegate_ctlc = True,
        Proc.std_in = Proc.NoStream,
        Proc.std_out = Proc.CreatePipe,
        Proc.std_err = Proc.NoStream
      }
    $ \_ stdout _ p -> do
      _ <- Proc.waitForProcess p
      Text.lines <$> maybe (pure "") Text.hGetContents stdout

processCommand :: Command -> AppEvent ()
processCommand (CmdShell waitForKey cmd) = do
  cmdSubstitutions cmd >>= suspendAndRunShellCommand waitForKey
  reloadDir
processCommand (CmdCommandShell cmd) = do
  stdout <- cmdSubstitutions cmd >>= liftIO . readCommandLines'
  forM_ stdout runIfCmd
  reloadDir
  where
    runIfCmd (Text.stripPrefix "<daffm>" -> Just cmd') =
      processCommand $ fromMaybe CmdNoop (parseCommand cmd')
    runIfCmd _ = pure ()
processCommand CmdQuit = M.halt
processCommand (CmdSetCmdline txt) = enterCmdline >> cmdSubstitutions txt >>= setCmdlineText
processCommand CmdEnterCmdline = enterCmdline
processCommand CmdLeaveCmdline = leaveCmdline
processCommand CmdOpenSelection = openSelectedFile
processCommand (CmdChangeDir dir) = cmdSubstitutions dir >>= changeDir
processCommand CmdReload = reloadDir
processCommand CmdToggleSelection = toggleCurrentFileSelection
processCommand CmdClearSelection = clearFileSelections
processCommand CmdGoBack = goBackToParentDir
processCommand (CmdChain chain) = forM_ chain processCommand
processCommand (CmdSearch term) = setSearchTerm term >> applySearch >> nextSearchMatch
processCommand (CmdSearchNext change) = updateSearchIndex (+ change) >> nextSearchMatch
processCommand (CmdKeymapSet keys command) =
  modify $ \st -> st {stateKeyMap = Map.insert keys command $ stateKeyMap st}
processCommand (CmdMove move) = moveCursor $ toUpdater move
  where
    toUpdater MoveToEnd = L.listMoveToEnd
    toUpdater (MoveTo pos) = L.listMoveTo pos
    toUpdater (MoveUp inc) = L.listMoveBy $ - inc
    toUpdater (MoveDown inc) = L.listMoveBy inc
    moveCursor :: (L.List FocusTarget FileInfo -> L.List FocusTarget FileInfo) -> AppEvent ()
    moveCursor updater = do
      files <- gets $ updater . stateFiles
      modify $ \st -> st {stateFiles = files}
processCommand CmdNoop = pure ()

evaluateCommand :: Text.Text -> AppEvent ()
evaluateCommand cmdtxt =
  case parseCommand cmdtxt of
    Just cmd -> processCommand cmd
    Nothing -> pure ()
