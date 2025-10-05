module Daffm.Types where

import Brick (EventM)
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Graphics.Vty as V
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
  { stateCmdlineEditor :: CmdlineEditor,
    stateCwd :: FilePathText,
    stateFileSelections :: Set.Set FilePathText,
    stateFiles :: L.List FocusTarget FileInfo,
    stateFocusTarget :: FocusTarget,
    stateKeyMap :: Keymap,
    stateKeySequence :: KeySequence,
    stateListPositionHistory :: Map.Map Text.Text Int,
    stateOpenerScript :: Maybe Text.Text
  }
  deriving (Show)

type AppEvent = EventM FocusTarget AppState

type CmdlineEditor = Editor.Editor Text.Text FocusTarget

data KeyMatchResult = MatchSuccess Command | MatchPartial | MatchFailure
  deriving (Show, Eq)

data Command
  = CmdShell Bool Text.Text
  | CmdQuit
  | CmdSetCmdline Text.Text
  | CmdEnterCmdline
  | CmdLeaveCmdline
  | CmdOpenSelection
  | CmdChangeDir Text.Text
  | CmdReload
  | CmdToggleSelection
  | CmdClearSelection
  | CmdGoBack
  | CmdChain [Command]
  | CmdNoop
  deriving (Show, Eq)

type Key = V.Key

type Keymap = Map.Map [Key] Command

type KeySequence = [Key]

data Configuration = Configuration
  { configKeymap :: !Keymap,
    configOpener :: Maybe Text.Text,
    configTheme :: !(Map.Map Text.Text Text.Text)
  }
  deriving (Show)

defaultConfiguration :: Configuration
defaultConfiguration = Configuration {configKeymap = Map.empty, configOpener = Nothing, configTheme = Map.empty}
