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
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import System.Directory (getHomeDirectory)
import System.Environment (getEnvironment)
import qualified System.Exit as Proc
import System.FilePath (takeDirectory)
import qualified System.Process as Proc

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

loadDir :: FilePathText -> AppEvent ()
loadDir dir = do
  modifyM (liftIO . (>>= filterInvalidSelections) . loadDirToState dir)
  applySearch -- Apply search after loading dir to update match indexes

reloadDir :: AppEvent ()
reloadDir = gets stateCwd >>= loadDir

goBackToParentDir :: AppEvent ()
goBackToParentDir = do
  parentDir <- gets (Text.pack . takeDirectory . Text.unpack . stateCwd)
  loadDir parentDir

changeDir :: FilePathText -> AppEvent ()
changeDir = loadDir

goHome :: AppEvent ()
goHome = liftIO getHomeDirectory >>= changeDir . Text.pack

openSelectedFile :: AppEvent ()
openSelectedFile = do
  currentFile >>= \case
    Just (FileInfo {filePath, fileType = Directory}) -> loadDir filePath
    Just (FileInfo {filePath, fileLinkType = Just Directory}) -> loadDir filePath
    Just _ -> do
      opener <- gets (fromMaybe "echo \"$files\" | xargs -i xdg-open {}" . stateOpenerScript)
      cmdSubstitutions opener >>= suspendAndRunShellCommand False
    Nothing -> pure ()

cmdSubstitutions :: Text.Text -> AppEvent Text.Text
cmdSubstitutions cmd = gets (`substitute` cmd)
  where
    escape = (\s -> "'" <> s <> "'") . Text.replace "'" "'\\''"
    substitute (AppState {stateFiles, stateCwd, stateFileSelections}) =
      Text.replace "%" (escape cursorFile)
        . Text.replace "%d" (escape stateCwd)
        . Text.replace "%s" (Text.unwords $ map escape selections)
        . Text.replace "%S" (Text.dropWhileEnd (== '\n') $ Text.unlines selections)
        . Text.replace "%f" (Text.unwords $ map escape selectionsOrCursor)
        . Text.replace "%F" (Text.dropWhileEnd (== '\n') $ Text.unlines selectionsOrCursor)
      where
        cursorFile = maybe "" (filePath . snd) . L.listSelectedElement $ stateFiles
        selections = Set.elems stateFileSelections
        selectionsOrCursor = if Set.null stateFileSelections then [cursorFile] else selections

-- Suspend tui and run shell command
-- When waitForKey is true, it will prompt for a key press on success
-- When exit code is non-zero, it will print it and prompt for key press regardless of waitForKey
suspendAndRunShellCommand :: Bool -> Text.Text -> AppEvent ()
suspendAndRunShellCommand waitForKey cmd = do
  env <- gets geCommandEnvFromState
  suspendAndResume' $ do
    shellCommand (Text.unpack cmd) env >>= \case
      Proc.ExitFailure code -> do
        putStrLn $ "Process exited with " <> show code
        putStrLn "Press any key to continue" >> void getChar
      _ | waitForKey -> putStrLn "Press any key to continue" >> void getChar
      _ -> pure ()

geCommandEnvFromState :: AppState -> Map.Map String String
geCommandEnvFromState (AppState {stateFileSelections, stateFiles}) = do
  Map.fromList
    [ ("files", Text.unpack $ Text.dropWhileEnd (== '\n') $ Text.unlines selectionsOrCursor),
      ("selections", Text.unpack $ Text.dropWhileEnd (== '\n') $ Text.unlines selections),
      ("cursor", Text.unpack cursorFile)
    ]
  where
    cursorFile = maybe "" (filePath . snd) . L.listSelectedElement $ stateFiles
    selections = Set.elems stateFileSelections
    selectionsOrCursor = if Set.null stateFileSelections then [cursorFile] else selections

shellCommand :: String -> Map.Map String String -> IO Proc.ExitCode
shellCommand cmd env = do
  cmdEnv <- (++ Map.toList env) <$> getEnvironment
  Proc.withCreateProcess
    (Proc.shell cmd) {Proc.delegate_ctlc = True, Proc.env = Just cmdEnv}
    $ \_ _ _ p -> Proc.waitForProcess p

currentFile :: AppEvent (Maybe FileInfo)
currentFile = gets (fmap snd . L.listSelectedElement . stateFiles)

toggleCurrentFileSelection :: AppEvent ()
toggleCurrentFileSelection = do
  currentFile >>= \case
    Just fileInfo -> modify . toggleFileSelection . filePath $ fileInfo
    Nothing -> pure ()
  moveCurrent 1

addFileSelection :: FilePathText -> AppEvent ()
addFileSelection path = do
  modify $ \st -> st {stateFileSelections = Set.insert path . stateFileSelections $ st}

removeFileSelection :: FilePathText -> AppEvent ()
removeFileSelection path = do
  modify $ \st -> st {stateFileSelections = Set.delete path . stateFileSelections $ st}

clearFileSelections :: AppEvent ()
clearFileSelections =
  modify $ \st -> st {stateFileSelections = Set.empty}

moveCurrent :: Int -> AppEvent ()
moveCurrent count =
  modify $ \st -> st {stateFiles = L.listMoveBy count $ stateFiles st}

setSearchTerm :: Text.Text -> AppEvent ()
setSearchTerm "" = modify $ \st -> st {stateSearchTerm = Nothing, stateSearchIndex = 0}
setSearchTerm term = modify $ \st -> st {stateSearchTerm = Just term, stateSearchIndex = 0}

applySearch :: AppEvent ()
applySearch = get >>= search
  where
    search :: AppState -> AppEvent ()
    search (AppState {stateSearchTerm = Nothing}) =
      modify $ \st -> st {stateSearchMatches = Vec.empty, stateSearchIndex = 0}
    search (AppState {stateSearchTerm = Just term, stateFiles}) =
      modify $
        \st ->
          st
            { stateSearchMatches = searchFiles stateFiles,
              stateSearchIndex = wrapSearchIndex st $ stateSearchIndex st
            }
      where
        isAMatch (FileInfo {fileName}) = Text.toLower term `Text.isInfixOf` Text.toLower fileName
        searchFiles = Vec.map fst . Vec.filter (isAMatch . snd) . Vec.indexed . L.listElements

nextSearchMatch :: AppEvent ()
nextSearchMatch =
  modify (\st -> st {stateFiles = forwardSearch st})
  where
    forwardSearch st@(AppState {stateSearchMatches, stateFiles, stateSearchIndex}) =
      if Vec.null stateSearchMatches
        then stateFiles
        else L.listMoveTo (stateSearchMatches Vec.! wrapSearchIndex st stateSearchIndex) stateFiles

wrapSearchIndex :: AppState -> Int -> Int
wrapSearchIndex (AppState {stateSearchMatches}) nextIndex =
  if
    | nextIndex < 0 -> matchCount - 1
    | nextIndex >= matchCount && matchCount /= 0 -> nextIndex `mod` matchCount
    | otherwise -> nextIndex
  where
    matchCount = length stateSearchMatches

updateSearchIndex :: (Int -> Int) -> AppEvent ()
updateSearchIndex update =
  modify $ \st -> st {stateSearchIndex = wrapSearchIndex st $ update $ stateSearchIndex st}
