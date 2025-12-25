module Daffm.Types where

import Brick (EventM)
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import Control.Applicative ((<|>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vec
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
    fileType :: FileType,
    fileUser :: Text.Text,
    fileGroup :: Text.Text,
    fileLinkType :: Maybe FileType,
    fileLinkTarget :: Maybe FilePathText
  }
  deriving (Show)

data FocusTarget = FocusCmdline | FocusMain deriving (Show, Eq, Ord)

data AppState = AppState
  { stateCmdlineEditor :: CmdlineEditor,
    stateCwd :: FilePathText,
    stateCustomCommands :: Map.Map Text.Text Command,
    stateFileSelections :: Set.Set FilePathText,
    stateFiles :: L.List FocusTarget FileInfo,
    stateMessage :: Maybe Text.Text,
    stateFocusTarget :: FocusTarget,
    stateKeyMap :: Keymap,
    stateKeySequence :: KeySequence,
    stateListPositionHistory :: Map.Map Text.Text Int,
    stateOpenerScript :: Maybe Text.Text,
    stateSearchTerm :: Maybe Text.Text,
    stateSearchMatches :: Vec.Vector Int,
    stateSearchIndex :: Int
  }
  deriving (Show)

type CustomArgs = Text.Text

type AppEvent = EventM FocusTarget AppState

type CmdlineEditor = Editor.Editor Text.Text FocusTarget

data KeyMatchResult = MatchSuccess Command | MatchPartial | MatchFailure
  deriving (Show, Eq)

data MoveInc = MoveDown Int | MoveUp Int | MoveTo Int | MoveToEnd
  deriving (Show, Eq)

data Command
  = CmdShell Bool Text.Text
  | CmdCommandShell Text.Text
  | CmdQuit
  | CmdSetCmdline Text.Text
  | CmdEnterCmdline
  | CmdLeaveCmdline
  | CmdSelectionOpen
  | CmdChangeDir Text.Text
  | CmdReload
  | CmdSelectionToggle
  | CmdSelectionAdd Text.Text
  | CmdSelectionRemove Text.Text
  | CmdSelectionClear
  | CmdGoBack
  | CmdChain [Command]
  | CmdSearch Text.Text
  | CmdSearchNext Int
  | CmdKeymapSet [Key] Command
  | CmdMove MoveInc
  | CmdCustom Text.Text CustomArgs
  | CmdNoop
  deriving (Show, Eq)

type Key = V.Key

type Keymap = Map.Map [Key] Command

type KeySequence = [Key]

data Configuration = Configuration
  { configKeymap :: !Keymap,
    configOpener :: !(Maybe Text.Text),
    configExtend :: !(Maybe Text.Text),
    configCommands :: !(Map.Map Text.Text Command)
  }
  deriving (Show)

instance Semigroup Configuration where
  a <> b =
    a
      { configKeymap = configKeymap a <> configKeymap b,
        configOpener = configOpener a <|> configOpener b,
        configCommands = configCommands a <> configCommands b
      }

data Args = Args
  { argsDirOrFile :: Maybe Text.Text,
    argsConfigFile :: Maybe FilePath,
    argsHelp :: Bool
  }
  deriving (Show)
