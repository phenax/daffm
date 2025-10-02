module Daffm.Types where

import qualified Brick.Widgets.List as L
import System.Posix.Types (FileOffset)

data FileType
  = RegularFile
  | BlockDevice
  | CharacterDevice
  | NamedPipe
  | Directory
  | SymbolicLink
  | UnixSocket
  deriving (Show)

data FileInfo = FileInfo
  { fileName :: String,
    filePath :: FilePath,
    fileSize :: FileOffset,
    fileType :: FileType
  }
  deriving (Show)

data AppState = AppState
  { stateFiles :: L.List () FileInfo,
    stateCwd :: FilePath,
    stateParentDir :: FilePath
  }
  deriving (Show)
