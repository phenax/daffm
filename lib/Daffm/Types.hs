module Daffm.Types where

import Brick (EventM)
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import Control.Applicative ((<|>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import qualified Graphics.Vty as K
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
    fileLinkType :: Maybe FileType,
    fileLinkTarget :: Maybe FilePathText
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
    stateOpenerScript :: Maybe Text.Text,
    stateSearchTerm :: Maybe Text.Text,
    stateSearchMatches :: Vec.Vector Int,
    stateSearchIndex :: Int
  }
  deriving (Show)

type AppEvent = EventM FocusTarget AppState

type CmdlineEditor = Editor.Editor Text.Text FocusTarget

data KeyMatchResult = MatchSuccess Command | MatchPartial | MatchFailure
  deriving (Show, Eq)

data Command
  = CmdShell Bool Text.Text
  | CmdCommandShell Text.Text
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
  | CmdSearch Text.Text
  | CmdSearchNext Int
  | CmdKeymapSet [Key] Command
  | CmdNoop
  deriving (Show, Eq)

type Key = V.Key

type Keymap = Map.Map [Key] Command

type KeySequence = [Key]

data Configuration = Configuration
  { configKeymap :: !Keymap,
    configOpener :: !(Maybe Text.Text),
    configExtend :: !(Maybe Text.Text),
    configTheme :: !(Map.Map Text.Text Text.Text)
  }
  deriving (Show)

instance Semigroup Configuration where
  a <> b =
    a
      { configKeymap = configKeymap a <> configKeymap b,
        configOpener = configOpener a <|> configOpener b,
        configTheme = configTheme a <> configTheme b
      }

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    { configKeymap = defaultKeymaps,
      configOpener = Nothing,
      configTheme = Map.empty,
      configExtend = Nothing
    }

defaultKeymaps :: Keymap
defaultKeymaps =
  Map.fromList
    [ ([K.KChar 'q'], CmdQuit),
      ([K.KChar 'r', K.KChar 'r'], CmdReload),
      ([K.KChar '!'], CmdSetCmdline "!"),
      ([K.KChar '/'], CmdSetCmdline "search "),
      ([K.KChar 'n'], CmdSearchNext 1),
      ([K.KChar 'N'], CmdSearchNext (-1)),
      ([K.KChar ':'], CmdEnterCmdline),
      ([K.KChar 'l'], CmdOpenSelection),
      ([K.KChar 'h'], CmdGoBack),
      ([K.KEnter], CmdOpenSelection),
      ([K.KBS], CmdGoBack),
      ([K.KChar 'v'], CmdToggleSelection),
      ([K.KChar '\t'], CmdToggleSelection),
      ([K.KChar 'C'], CmdClearSelection),
      ([K.KChar '~'], CmdChangeDir "~"),
      ([K.KChar '$'], CmdShell False "$SHELL"),
      ([K.KChar 'g', K.KChar 'x'], CmdShell False "!xdg-open % >/dev/null 2>&1"),
      ([K.KChar 'g', K.KChar 'h'], CmdChangeDir "~"),
      ([K.KChar 'g', K.KChar 'c', K.KChar 'f', K.KChar 'g'], CmdChangeDir "~/.config/daffm")
    ]
