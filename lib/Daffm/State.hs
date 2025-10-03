module Daffm.State where

import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import Control.Applicative ((<|>))
import Control.Monad (forM)
import Daffm.Types (AppState (..), FileInfo (..), FileType (..), FocusTarget (..))
import Data.Char (toLower)
import Data.List (findIndex, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec
import System.Directory (listDirectory, makeAbsolute, setCurrentDirectory)
import qualified System.PosixCompat as Posix

mkEmptyAppState :: AppState
mkEmptyAppState =
  AppState
    { stateFiles = L.list FocusMain (Vec.fromList []) 1,
      stateCmdlineEditor = Editor.editor FocusCmdline Nothing "",
      stateFocusTarget = FocusMain,
      stateListPositionCache = Map.empty,
      stateCwd = "",
      stateParentDir = ""
    }

loadDirInAppState :: FilePath -> FilePath -> AppState -> IO AppState
loadDirInAppState dir parentDir appState@(AppState {stateCwd, stateListPositionCache}) = do
  setCurrentDirectory dir
  files <- listFilesInDir dir
  let prevDirPosM = findIndex ((== stateCwd) . filePath) files
  let cachedPosM = Map.lookup dir stateListPositionCache
  let pos = fromMaybe 0 (cachedPosM <|> prevDirPosM)
  let list = L.listMoveTo pos $ L.list FocusMain (Vec.fromList files) 1
  pure $
    appState
      { stateFiles = list,
        stateCwd = dir,
        stateParentDir = parentDir
      }

fileTypeFromStatus :: Posix.FileStatus -> FileType
fileTypeFromStatus s =
  if
    | Posix.isBlockDevice s -> BlockDevice
    | Posix.isCharacterDevice s -> CharacterDevice
    | Posix.isNamedPipe s -> NamedPipe
    | Posix.isRegularFile s -> RegularFile
    | Posix.isDirectory s -> Directory
    | Posix.isSocket s -> UnixSocket
    | Posix.isSymbolicLink s -> SymbolicLink
    | otherwise -> UnknownFileType

getFileInfo :: FilePath -> IO FileInfo
getFileInfo name = do
  path <- makeAbsolute name
  stat <- Posix.getSymbolicLinkStatus path
  pure $
    FileInfo
      { filePath = path,
        fileName = name,
        fileSize = Posix.fileSize stat,
        fileType = fileTypeFromStatus stat
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

cacheDirPosition :: AppState -> AppState
cacheDirPosition appState@(AppState {stateListPositionCache, stateCwd, stateFiles}) =
  appState
    { stateListPositionCache = Map.insert stateCwd pos stateListPositionCache
    }
  where
    pos = fromMaybe 0 $ L.listSelected stateFiles
