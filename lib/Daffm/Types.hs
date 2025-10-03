module Daffm.Types where

import Brick (EventM)
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
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

type FilePathText = Text.Text

data FileInfo = FileInfo
  { fileName :: Text.Text,
    filePath :: FilePathText,
    fileSize :: FileOffset,
    fileMode :: FileMode,
    fileType :: FileType
  }
  deriving (Show)

data FocusTarget = FocusCmdline | FocusMain deriving (Show, Eq, Ord)

data AppState = AppState
  { stateFiles :: L.List FocusTarget FileInfo,
    stateCmdlineEditor :: CmdlineEditor,
    stateFileSelections :: Set.Set FilePathText,
    stateFocusTarget :: FocusTarget,
    stateCwd :: FilePathText,
    stateListPositionCache :: Map.Map Text.Text Int,
    stateParentDir :: FilePathText
  }
  deriving (Show)

type AppEvent = EventM FocusTarget AppState

type CmdlineEditor = Editor.Editor Text.Text FocusTarget
