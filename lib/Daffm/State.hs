module Daffm.State where

import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import Control.Applicative ((<|>))
import Control.Monad (filterM, forM)
import Daffm.Types (AppState (..), Command (..), FileInfo (..), FilePathText, FileType (..), FocusTarget (..))
import Data.List (findIndex, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Zipper.Generic as Zipper
import qualified Data.Vector as Vec
import qualified Graphics.Vty as K
import System.Directory (listDirectory, makeAbsolute, setCurrentDirectory)
import System.PosixCompat (fileExist)
import qualified System.PosixCompat as Posix

mkEditor :: (Zipper.GenericTextZipper a) => a -> Editor.Editor a FocusTarget
mkEditor = Editor.editor FocusCmdline (Just 1)

mkEmptyAppState :: AppState
mkEmptyAppState =
  AppState
    { stateFiles = L.list FocusMain (Vec.fromList []) 1,
      stateCmdlineEditor = mkEditor "",
      stateFocusTarget = FocusMain,
      stateListPositionCache = Map.empty,
      stateFileSelections = Set.empty,
      stateCwd = "",
      stateParentDir = "",
      stateKeyMap = defaultKeymaps,
      stateKeySequence = []
    }
  where
    defaultKeymaps =
      Map.fromList
        [ ([K.KChar 'q'], CmdQuit),
          ([K.KChar 'r', K.KChar 'r'], CmdReload),
          ([K.KChar '!'], CmdSetCmdline "!"),
          ([K.KChar ':'], CmdEnterCmdline),
          ([K.KChar 'l'], CmdOpenSelection),
          ([K.KChar 'h'], CmdGoBack),
          ([K.KEnter], CmdOpenSelection),
          ([K.KBS], CmdGoBack),
          ([K.KChar 'v'], CmdToggleSelection),
          ([K.KChar '\t'], CmdToggleSelection),
          ([K.KChar 'C'], CmdClearSelection),
          ([K.KChar '~'], CmdChangeDir "/home/imsohexy"),
          ([K.KChar 'g', K.KChar 'h'], CmdChangeDir "/home/imsohexy"),
          ([K.KChar 'g', K.KChar 'd', K.KChar 'c'], CmdChangeDir "/home/imsohexy/Documents"),
          ([K.KChar 'g', K.KChar 'd', K.KChar 'l'], CmdChangeDir "/home/imsohexy/Downloads"),
          ([K.KChar 'g', K.KChar 'p'], CmdChangeDir "/home/imsohexy/Pictures"),
          -- Just for testing
          ([K.KChar 'p'], CmdShell True "chafa -f kitty %")
        ]

toggleSetItem :: (Ord a) => a -> Set.Set a -> Set.Set a
toggleSetItem val set =
  if val `Set.member` set then Set.delete val set else Set.insert val set

toggleFileSelection :: FilePathText -> AppState -> AppState
toggleFileSelection path st = st {stateFileSelections = toggleSetItem path $ stateFileSelections st}

loadDirToState :: FilePathText -> FilePathText -> AppState -> IO AppState
loadDirToState dir parentDir appState@(AppState {stateCwd, stateListPositionCache}) = do
  setCurrentDirectory $ Text.unpack dir
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

getFileInfo :: FilePathText -> IO FileInfo
getFileInfo name = do
  path <- makeAbsolute $ Text.unpack name
  stat <- Posix.getSymbolicLinkStatus path
  pure $
    FileInfo
      { filePath = Text.pack path,
        fileName = name,
        fileSize = Posix.fileSize stat,
        fileMode = Posix.fileMode stat,
        fileType = fileTypeFromStatus stat
      }

fileSorter :: FileInfo -> FileInfo -> Ordering
fileSorter (FileInfo {fileType = Directory, fileName = fa}) (FileInfo {fileType = Directory, fileName = fb}) =
  compare (Text.toLower fa) (Text.toLower fb)
fileSorter (FileInfo {fileType = Directory}) _ = LT
fileSorter _ (FileInfo {fileType = Directory}) = GT
fileSorter (FileInfo {fileName = fa}) (FileInfo {fileName = fb}) =
  compare (Text.toLower fa) (Text.toLower fb)

listFilesInDir :: FilePathText -> IO [FileInfo]
listFilesInDir dir = do
  files <- listDirectory (Text.unpack dir)
  sortBy fileSorter <$> forM files (getFileInfo . Text.pack)

cacheDirPosition :: AppState -> AppState
cacheDirPosition appState@(AppState {stateListPositionCache, stateCwd, stateFiles}) =
  appState
    { stateListPositionCache = Map.insert stateCwd pos stateListPositionCache
    }
  where
    pos = fromMaybe 0 $ L.listSelected stateFiles

filterInvalidSelections :: AppState -> IO AppState
filterInvalidSelections st = do
  selections <- filterM (fileExist . Text.unpack) . Set.elems $ stateFileSelections st
  pure $ st {stateFileSelections = Set.fromList selections}
