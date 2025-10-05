{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use for_" #-}
{-# HLINT ignore "Use <=<" #-}
module Daffm.Action.Core where

import Brick (suspendAndResume')
import qualified Brick.Widgets.List as L
import Control.Monad (void)
import Control.Monad.State (MonadIO (liftIO), MonadState, get, gets, modify, put)
import Daffm.State
import Daffm.Types (AppEvent, AppState (..), FileInfo (..), FilePathText, FileType (..))
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Directory (getHomeDirectory)
import qualified System.Exit as Proc
import System.FilePath (takeDirectory)
import qualified System.Process as Proc

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

loadDir :: FilePathText -> AppEvent ()
loadDir dir = do
  modifyM (liftIO . (>>= filterInvalidSelections) . loadDirToState dir)

reloadDir :: AppEvent ()
reloadDir = do
  AppState {stateCwd} <- get
  loadDir stateCwd

goBackToParentDir :: AppEvent ()
goBackToParentDir = do
  dir <- gets (Text.pack . takeDirectory . Text.unpack . stateCwd)
  loadDir dir

changeDir :: FilePathText -> AppEvent ()
changeDir = loadDir

goHome :: AppEvent ()
goHome = do
  liftIO getHomeDirectory >>= changeDir . Text.pack

openSelectedFile :: AppEvent ()
openSelectedFile = do
  currentFile >>= \case
    Just (FileInfo {filePath, fileType = Directory}) -> loadDir filePath
    Just _ -> do
      opener <- gets (fromMaybe "echo '%F' | xargs -i xdg-open {}" . stateOpenerScript)
      cmdSubstitutions opener >>= suspendAndRunShellCommand False
    Nothing -> pure ()

shellCommand :: String -> IO Proc.ExitCode
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
        Text.replace "%" (escape file)
          . Text.replace "%d" (escape stateCwd)
          . Text.replace "%s" (Text.unwords $ map escape selections)
          . Text.replace "%S" (Text.dropWhileEnd (== '\n') $ Text.unlines selections)
          . Text.replace "%f" (Text.unwords $ map escape selectionsOrCurrent)
          . Text.replace "%F" (Text.dropWhileEnd (== '\n') $ Text.unlines selectionsOrCurrent)
  pure . subst $ cmd

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

currentFile :: AppEvent (Maybe FileInfo)
currentFile = do
  gets (fmap snd . L.listSelectedElement . stateFiles)

toggleCurrentFileSelection :: AppEvent ()
toggleCurrentFileSelection = do
  currentFile >>= \case
    Just file -> modify $ toggleFileSelection (filePath file)
    Nothing -> pure ()
  moveCurrent 1

clearFileSelections :: AppEvent ()
clearFileSelections =
  modify $ \s -> s {stateFileSelections = Set.empty}

moveCurrent :: Int -> AppEvent ()
moveCurrent count = do
  files <- gets stateFiles
  modify $ \s -> s {stateFiles = L.listMoveBy count files}
