module Daffm where

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import Control.Monad (forM)
import Control.Monad.State (MonadIO (liftIO), MonadState, get, gets, modify, put)
import Daffm.Attrs (appAttrMap)
import Daffm.Types (AppState (..), FileInfo (..), FileType (..), FocusTarget (FocusCmdline, FocusMain))
import Daffm.View (appView)
import Data.Char (toLower)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Vector ((!?))
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import System.Directory (listDirectory, makeAbsolute, setCurrentDirectory)
import System.FilePath (takeDirectory)
import qualified System.PosixCompat as Posix

type AppEvent = T.EventM FocusTarget AppState

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

openSelectedFile :: AppEvent ()
openSelectedFile = do
  AppState {stateFiles, stateCwd} <- get
  let indexM = L.listSelected stateFiles
  let files = L.listElements stateFiles
  case indexM >>= (files !?) of
    Just (FileInfo {filePath, fileType = Directory}) ->
      modifyM (liftIO . loadDirInAppState filePath stateCwd)
    Just (FileInfo {filePath, fileType}) -> do
      liftIO . putStrLn $ "Opening " <> show fileType <> ": " <> filePath
      pure ()
    Nothing -> pure ()
  pure ()

goBackToParentDir :: AppEvent ()
goBackToParentDir = do
  dir <- gets stateParentDir
  modifyM (liftIO . loadDirInAppState dir (takeDirectory dir))

appEvent :: T.BrickEvent FocusTarget e -> AppEvent ()
appEvent brickevent@(T.VtyEvent event) = do
  focusTarget <- gets stateFocusTarget
  case (focusTarget, event) of
    (_, V.EvKey V.KEsc []) -> modify (\st -> st {stateFocusTarget = FocusMain})
    (_, V.EvKey (V.KChar ':') []) -> modify (\st -> st {stateFocusTarget = FocusCmdline})
    (FocusMain, V.EvKey (V.KChar 'q') []) -> M.halt
    (FocusMain, V.EvKey (V.KChar 'l') []) -> openSelectedFile
    (FocusMain, V.EvKey (V.KChar 'h') []) -> goBackToParentDir
    (FocusMain, V.EvKey V.KEnter []) -> openSelectedFile
    (FocusMain, V.EvKey V.KBS []) -> goBackToParentDir
    (FocusMain, _) -> do
      files <- gets stateFiles
      newFiles <- T.nestEventM' files (L.handleListEventVi L.handleListEvent event)
      modify (\appState -> appState {stateFiles = newFiles})
    (FocusCmdline, _) -> do
      editor <- gets stateCmdlineEditor
      newEditor <- T.nestEventM' editor (Editor.handleEditorEvent brickevent)
      modify (\appState -> appState {stateCmdlineEditor = newEditor})
appEvent _ = pure ()

app :: M.App AppState e FocusTarget
app =
  M.App
    { M.appDraw = appView,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = pure (),
      M.appAttrMap = const appAttrMap
    }

fileTypeFromStatus :: Posix.FileStatus -> Maybe FileType
fileTypeFromStatus s =
  if
    | Posix.isBlockDevice s -> Just BlockDevice
    | Posix.isCharacterDevice s -> Just CharacterDevice
    | Posix.isNamedPipe s -> Just NamedPipe
    | Posix.isRegularFile s -> Just RegularFile
    | Posix.isDirectory s -> Just Directory
    | Posix.isSocket s -> Just UnixSocket
    | Posix.isSymbolicLink s -> Just SymbolicLink
    | otherwise -> Nothing

getFileInfo :: FilePath -> IO FileInfo
getFileInfo name = do
  path <- makeAbsolute name
  stat <- Posix.getSymbolicLinkStatus path
  pure $
    FileInfo
      { filePath = path,
        fileName = name,
        fileSize = Posix.fileSize stat,
        fileType = fromMaybe RegularFile $ fileTypeFromStatus stat
      }

fileSorter :: FileInfo -> FileInfo -> Ordering
fileSorter (FileInfo {fileType = Directory, fileName = fa}) (FileInfo {fileType = Directory, fileName = fb}) =
  compare (toLower <$> fa) (toLower <$> fb)
fileSorter (FileInfo {fileType = Directory}) _ = LT
fileSorter _ (FileInfo {fileType = Directory}) = GT
fileSorter (FileInfo {fileName = fa}) (FileInfo {fileName = fb}) =
  compare (toLower <$> fa) (toLower <$> fb)

listFilesInDir :: FilePath -> IO [FileInfo]
listFilesInDir dir = do
  files <- listDirectory dir
  sortBy fileSorter <$> forM files getFileInfo

loadDirInAppState :: FilePath -> FilePath -> AppState -> IO AppState
loadDirInAppState dir parentDir appState = do
  setCurrentDirectory dir
  files <- listFilesInDir dir
  pure $
    appState
      { stateFiles = L.list FocusMain (Vec.fromList files) 1,
        stateCwd = dir,
        stateParentDir = parentDir
      }

mkEmptyAppState :: AppState
mkEmptyAppState =
  AppState
    { stateFiles = L.list FocusMain (Vec.fromList []) 1,
      stateCmdlineEditor = Editor.editor FocusCmdline Nothing "",
      stateFocusTarget = FocusMain,
      -- stateFocusRing = focusRing [FocusMain, FocusCmdline],
      stateCwd = "",
      stateParentDir = ""
    }
