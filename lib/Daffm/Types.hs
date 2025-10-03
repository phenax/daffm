module Daffm.Types where

import Brick (EventM)
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Posix.Types (FileMode, FileOffset)

data FileType
  = RegularFile
  | BlockDevice
  | CharacterDevice
  | NamedPipe
  | Directory
  | SymbolicLink
  | UnixSocket
  | UnknownFileType
  deriving (Show)

data FileInfo = FileInfo
  { fileName :: String,
    filePath :: FilePath,
    fileSize :: FileOffset,
    fileMode :: FileMode,
    fileType :: FileType
  }
  deriving (Show)

data FocusTarget = FocusCmdline | FocusMain deriving (Show, Eq, Ord)

data AppState = AppState
  { stateFiles :: L.List FocusTarget FileInfo,
    stateCmdlineEditor :: CmdlineEditor,
    stateFileSelections :: Set.Set FilePath,
    stateFocusTarget :: FocusTarget,
    stateCwd :: FilePath,
    stateListPositionCache :: Map.Map String Int,
    stateParentDir :: FilePath
  }
  deriving (Show)

type AppEvent = EventM FocusTarget AppState

type CmdlineEditor = Editor.Editor String FocusTarget
