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
import System.Environment (getEnvironment)

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
      ("open", _) -> Just CmdSelectionOpen
      ("reload", _) -> Just CmdReload
      ("cd", dir) -> Just $ CmdChangeDir dir
      ("noop", _) -> Just CmdNoop
      ("cmdline-enter", _) -> Just CmdEnterCmdline
      ("cmdline-leave", _) -> Just CmdLeaveCmdline
      ("cmdline-set", txt) -> Just $ CmdSetCmdline txt
      ("selection-toggle", _) -> Just CmdSelectionToggle
      ("selection-clear", _) -> Just CmdSelectionClear
      ("selection-add", file) -> Just $ CmdSelectionAdd file
      ("selection-remove", file) -> Just $ CmdSelectionRemove file
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
      ("", _) -> Nothing
      (cmd', args) -> Just $ CmdCustom cmd' args

readCommandLines :: Map.Map String String -> Text.Text -> IO [Text.Text]
readCommandLines env cmd = do
  cmdEnv <- (++ Map.toList env) <$> getEnvironment
  Proc.withCreateProcess
    (Proc.shell $ Text.unpack cmd)
      { Proc.delegate_ctlc = True,
        Proc.std_in = Proc.NoStream,
        Proc.std_out = Proc.CreatePipe,
        Proc.std_err = Proc.NoStream,
        Proc.env = Just cmdEnv
      }
    $ \_ stdout _ p -> do
      _ <- Proc.waitForProcess p
      Text.lines <$> maybe (pure "") Text.hGetContents stdout

argSubst :: Text.Text -> Text.Text -> Text.Text
argSubst = Text.replace "%args"

processCommand :: Command -> CustomArgs -> AppEvent ()
processCommand (CmdShell waitForKey cmd) args = do
  cmdSubstitutions (argSubst args cmd) >>= suspendAndRunShellCommand waitForKey
  reloadDir
processCommand (CmdCommandShell cmd) args = do
  env <- gets geCommandEnvFromState
  stdout <- cmdSubstitutions (argSubst args cmd) >>= liftIO . readCommandLines env
  forM_ stdout runIfCmd
  reloadDir
  where
    runIfCmd (Text.stripPrefix "<daffm>" -> Just cmd') =
      processCommand (fromMaybe CmdNoop (parseCommand cmd')) args
    runIfCmd _ = pure ()
processCommand CmdQuit _args = M.halt
processCommand (CmdSetCmdline txt) args =
  enterCmdline >> cmdSubstitutions (argSubst args txt) >>= setCmdlineText
processCommand CmdEnterCmdline _args = enterCmdline
processCommand CmdLeaveCmdline _args = leaveCmdline
processCommand CmdSelectionOpen _args = openSelectedFile
processCommand (CmdChangeDir dir) args = cmdSubstitutions (argSubst args dir) >>= changeDir
processCommand CmdReload _args = reloadDir
processCommand CmdSelectionToggle _args = toggleCurrentFileSelection
processCommand CmdSelectionClear _args = clearFileSelections
processCommand (CmdSelectionAdd file) _args = addFileSelection $ trim file -- TODO: Subst
processCommand (CmdSelectionRemove file) _args = removeFileSelection $ trim file -- TODO: Subst
processCommand CmdGoBack _ = goBackToParentDir
processCommand (CmdChain chain) args = forM_ chain (`processCommand` args)
processCommand (CmdSearch term) _ = setSearchTerm term >> applySearch >> nextSearchMatch
processCommand (CmdSearchNext change) _ = updateSearchIndex (+ change) >> nextSearchMatch
processCommand (CmdKeymapSet keys command) _ =
  modify $ \st -> st {stateKeyMap = Map.insert keys command $ stateKeyMap st}
processCommand (CmdMove move) _ = moveCursor $ toUpdater move
  where
    toUpdater MoveToEnd = L.listMoveToEnd
    toUpdater (MoveTo pos) = L.listMoveTo pos
    toUpdater (MoveUp inc) = L.listMoveBy $ -inc
    toUpdater (MoveDown inc) = L.listMoveBy inc
    moveCursor :: (L.List FocusTarget FileInfo -> L.List FocusTarget FileInfo) -> AppEvent ()
    moveCursor updater = do
      files <- gets $ updater . stateFiles
      modify $ \st -> st {stateFiles = files}
processCommand (CmdCustom cmd args) _ = do
  myCmd <- gets $ Map.lookup cmd . stateCustomCommands
  case myCmd of
    Just command -> processCommand command args
    Nothing -> modify (\st -> st {stateMessage = Just $ "Invalid command " <> cmd})
processCommand CmdNoop _ = pure ()

evaluateCommand :: Text.Text -> AppEvent ()
evaluateCommand cmdtxt = do
  modify (\st -> st {stateMessage = Nothing})
  case parseCommand cmdtxt of
    Just cmd -> processCommand cmd ""
    Nothing -> pure ()
