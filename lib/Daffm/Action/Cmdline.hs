module Daffm.Action.Cmdline where

import Brick (suspendAndResume')
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import Control.Monad (unless, void)
import Control.Monad.State (get, gets, modify)
import Daffm.Action.Core (reloadDir)
import Daffm.Types (AppEvent, AppState (..), FileInfo (..), FocusTarget (..))
import Data.Char (isSpace)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Zipper as Z
import qualified Data.Text.Zipper as Zipper
import System.Process (callCommand, callProcess)

leaveCmdline :: AppEvent ()
leaveCmdline = clearCmdline >> modify (\st -> st {stateFocusTarget = FocusMain})

enterCmdline :: AppEvent ()
enterCmdline = modify (\st -> st {stateFocusTarget = FocusCmdline})

setCmdlineText :: Text.Text -> AppEvent ()
setCmdlineText text =
  applyCmdlineEdit (const $ Z.textZipper [text] (Just 1))

clearCmdline :: AppEvent ()
clearCmdline = applyCmdlineEdit Z.clearZipper

runCmdline :: AppEvent ()
runCmdline = do
  cmd <- gets (trimCmd . Editor.getEditContents . stateCmdlineEditor)
  evaluateCommand cmd
  leaveCmdline
  where
    trimCmd = Text.dropWhile isSpace . Text.dropWhileEnd isSpace . Text.unlines

evaluateCommand :: Text.Text -> AppEvent ()
evaluateCommand (Text.splitAt 2 -> ("!!", cmd)) = do
  cmd' <- Text.unpack <$> cmdSubstitutions cmd
  suspendAndResume' $ do
    callCommand cmd'
    putStrLn "Press any key to continue" >> void getChar
  reloadDir
evaluateCommand (Text.splitAt 1 -> ("!", cmd)) = do
  cmd' <- Text.unpack <$> cmdSubstitutions cmd
  suspendAndResume' $ callCommand cmd'
  reloadDir
evaluateCommand "delete" = do
  (AppState {stateFileSelections, stateFiles}) <- get
  let files =
        if Set.null stateFileSelections
          then maybe [] ((: []) . filePath . snd) $ L.listSelectedElement stateFiles
          else Set.elems stateFileSelections
  unless (null files) $ do
    suspendAndResume' $ callProcess "rm" ("-rfi" : map Text.unpack files)
  reloadDir
evaluateCommand _cmd = pure ()

cmdSubstitutions :: Text.Text -> AppEvent Text.Text
cmdSubstitutions cmd = do
  (AppState {stateFiles, stateCwd, stateFileSelections}) <- get
  let file = maybe "" (filePath . snd) . L.listSelectedElement $ stateFiles
  let escape = (\s -> "'" <> s <> "'") . Text.replace "'" "\\'"
  let selections = Set.elems stateFileSelections
  -- TODO: Escaping %
  let subst =
        Text.replace "%" file
          . Text.replace "%d" stateCwd
          . Text.replace "%s" (Text.unwords $ map escape selections)
          . Text.replace "%S" (Text.dropWhileEnd (== '\n') $ Text.unlines selections)
  pure . subst $ cmd

applyCmdlineEdit :: (Zipper.TextZipper Text.Text -> Zipper.TextZipper Text.Text) -> AppEvent ()
applyCmdlineEdit zipper = do
  editor <- gets stateCmdlineEditor
  let editor' = Editor.applyEdit zipper editor
  modify (\s -> s {stateCmdlineEditor = editor'})
