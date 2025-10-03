module Daffm.Action.Cmdline where

import Brick (suspendAndResume')
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import Control.Monad (void)
import Control.Monad.State (get, gets, modify)
import Daffm.Action.Core (reloadDir)
import Daffm.Types (AppEvent, AppState (..), FileInfo (..), FocusTarget (..))
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Text as Text
import qualified Data.Text.Zipper as Z
import qualified Data.Text.Zipper as Zipper
import System.Process (callCommand)

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
    putStrLn "Press any key to continue" >> void getChar
  reloadDir
evaluateCommand ('!' : cmd) = do
  cmd' <- Text.unpack <$> cmdSubstitutions (Text.pack cmd)
  suspendAndResume' $ callCommand cmd'
  reloadDir
evaluateCommand _cmd = pure ()

applyCmdlineEdit :: (Zipper.TextZipper String -> Zipper.TextZipper String) -> AppEvent ()
applyCmdlineEdit zipper = do
  editor <- gets stateCmdlineEditor
  let editor' = Editor.applyEdit zipper editor
  modify (\s -> s {stateCmdlineEditor = editor'})
