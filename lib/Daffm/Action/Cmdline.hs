module Daffm.Action.Cmdline where

import qualified Brick.Widgets.Edit as Editor
import Control.Monad.State (gets, modify)
import qualified Control.Monad.State.Strict as StateStrict
import Daffm.Types
import Daffm.Utils (trimStart)
import qualified Data.Text as Text
import qualified Data.Text.Zipper as Z
import qualified Data.Text.Zipper as Zipper

getCmdlineText :: AppEvent Text.Text
getCmdlineText = StateStrict.gets cmdtext
  where
    cmdtext = trimStart . Text.unlines . Editor.getEditContents . stateCmdlineEditor

leaveCmdline :: AppEvent ()
leaveCmdline = clearCmdline >> modify (\st -> st {stateFocusTarget = FocusMain})

enterCmdline :: AppEvent ()
enterCmdline = modify (\st -> st {stateFocusTarget = FocusCmdline})

setCmdlineText :: Text.Text -> AppEvent ()
setCmdlineText text =
  applyCmdlineEdit (const $ Z.textZipper [text] (Just 1))

clearCmdline :: AppEvent ()
clearCmdline = applyCmdlineEdit Z.clearZipper

applyCmdlineEdit :: (Zipper.TextZipper Text.Text -> Zipper.TextZipper Text.Text) -> AppEvent ()
applyCmdlineEdit zipper = do
  editor <- gets stateCmdlineEditor
  let editor' = Editor.applyEdit zipper editor
  modify (\s -> s {stateCmdlineEditor = editor'})
