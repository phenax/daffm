module Daffm.Types where

import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import qualified Data.Map as Map
import System.Posix.Types (FileOffset)

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
    fileType :: FileType
  }
  deriving (Show)

data FocusTarget = FocusCmdline | FocusMain deriving (Show, Eq, Ord)

data AppState = AppState
  { stateFiles :: L.List FocusTarget FileInfo,
    stateCmdlineEditor :: Editor.Editor String FocusTarget,
    stateFocusTarget :: FocusTarget,
    stateCwd :: FilePath,
    stateListPositionCache :: Map.Map String Int,
    stateParentDir :: FilePath
  }
  deriving (Show)
