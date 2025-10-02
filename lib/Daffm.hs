module Daffm where

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import Control.Monad (forM)
import Control.Monad.State (MonadIO (liftIO), get, gets, modify)
import Daffm.Attrs (appAttrMap)
import Daffm.Types (AppState (..), FileInfo (..), FileType (..))
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

openSelectedFile :: T.EventM () AppState ()
openSelectedFile = do
  state <- get
  let indexM = L.listSelected . stateFiles $ state
  let files = L.listElements . stateFiles $ state
  let fileM = indexM >>= (files !?)
  case fileM of
    Just (FileInfo {filePath, fileType = Directory}) -> do
      let parentDir = stateCwd state
      nextState <- liftIO $ loadDirInAppState filePath parentDir state
      modify (const nextState)
      pure ()
    Just (FileInfo {filePath, fileType}) -> do
      liftIO . putStrLn $ "Opening " <> show fileType <> ": " <> filePath
      pure ()
    Nothing -> pure ()
  pure ()

goBackToParentDir :: T.EventM () AppState ()
goBackToParentDir = do
  state <- get
  let dir = stateParentDir state
  let parentDir = takeDirectory dir
  nextState <- liftIO $ loadDirInAppState dir parentDir state
  modify (const nextState)

appEvent :: T.BrickEvent () e -> T.EventM () AppState ()
appEvent (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt
    V.EvKey (V.KChar 'q') [] -> M.halt
    V.EvKey (V.KChar 'l') [] -> openSelectedFile
    V.EvKey (V.KChar 'h') [] -> goBackToParentDir
    V.EvKey V.KEnter [] -> openSelectedFile
    V.EvKey V.KBS [] -> goBackToParentDir
    ev -> do
      files <- gets stateFiles
      newFiles <- T.nestEventM' files (L.handleListEventVi L.handleListEvent ev)
      modify (\appState -> appState {stateFiles = newFiles})
appEvent _ = pure ()

app :: M.App AppState e ()
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
      { stateFiles = L.list () (Vec.fromList files) 1,
        stateCwd = dir,
        stateParentDir = parentDir
      }

mkEmptyAppState :: AppState
mkEmptyAppState =
  AppState
    { stateFiles = L.list () (Vec.fromList []) 1,
      stateCwd = "",
      stateParentDir = ""
    }
