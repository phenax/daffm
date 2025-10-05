module Daffm.State where

import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import Control.Applicative ((<|>))
import Control.Monad (filterM, forM)
import Daffm.Types
import Daffm.Utils (trim)
import Data.List (findIndex, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Zipper.Generic as Zipper
import qualified Data.Vector as Vec
import qualified Graphics.Vty as K
import System.Directory (doesDirectoryExist, doesPathExist, getCurrentDirectory, getHomeDirectory, listDirectory, makeAbsolute, setCurrentDirectory)
import System.FilePath (joinPath, takeDirectory, takeFileName)
import System.PosixCompat (fileExist)
import qualified System.PosixCompat as Posix

defaultKeymaps :: Keymap
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
      ([K.KChar '~'], CmdChangeDir "~"),
      ([K.KChar '$'], CmdShell False "$SHELL"),
      ([K.KChar 'g', K.KChar 'x'], CmdShell False "!xdg-open % >/dev/null 2>&1"),
      ([K.KChar 'g', K.KChar 'h'], CmdChangeDir "~"),
      ([K.KChar 'g', K.KChar 'c', K.KChar 'f', K.KChar 'g'], CmdChangeDir "~/.config/daffm")
    ]

mkEditor :: (Zipper.GenericTextZipper a) => a -> Editor.Editor a FocusTarget
mkEditor = Editor.editor FocusCmdline (Just 1)

mkEmptyAppState :: Configuration -> AppState
mkEmptyAppState config =
  AppState
    { stateFiles = L.list FocusMain (Vec.fromList []) 1,
      stateCmdlineEditor = mkEditor "",
      stateFocusTarget = FocusMain,
      stateListPositionHistory = Map.empty,
      stateFileSelections = Set.empty,
      stateCwd = "",
      stateKeyMap = configKeymap config <> defaultKeymaps,
      stateOpenerScript = configOpener config,
      stateKeySequence = []
    }

toggleSetItem :: (Ord a) => a -> Set.Set a -> Set.Set a
toggleSetItem val set =
  if Set.member val set then Set.delete val set else Set.insert val set

toggleFileSelection :: FilePathText -> AppState -> AppState
toggleFileSelection path st = st {stateFileSelections = toggleSetItem path $ stateFileSelections st}

normalizePath :: FilePathText -> IO FilePathText
normalizePath "~" = Text.pack <$> getHomeDirectory
normalizePath (Text.stripPrefix "~/" -> (Just rest)) = do
  home <- getHomeDirectory
  pure . Text.pack . joinPath $ [home, Text.unpack rest]
normalizePath dir = Text.pack <$> makeAbsolute (Text.unpack dir)

withCwdFallback :: FilePathText -> IO FilePathText
withCwdFallback path = do
  exists <- doesPathExist $ Text.unpack path
  if exists then pure path else Text.pack <$> getCurrentDirectory

stripQuotes :: Text.Text -> Text.Text
stripQuotes txt = fromMaybe txt (double <|> single)
  where
    double = Text.stripPrefix "\"" txt >>= Text.stripSuffix "\""
    single = Text.stripPrefix "'" txt >>= Text.stripSuffix "'"

textAsString :: (String -> String) -> Text.Text -> Text.Text
textAsString f = Text.pack . f . Text.unpack

loadDirToState :: FilePathText -> AppState -> IO AppState
loadDirToState dir' appState@(AppState {stateCwd, stateListPositionHistory}) = do
  normalizedDir <- (normalizePath . stripQuotes . trim) dir' >>= withCwdFallback
  stat <- Posix.getSymbolicLinkStatus $ Text.unpack normalizedDir
  let (dir, targetFilePathM) =
        if Posix.isDirectory stat
          then (normalizedDir, Nothing)
          else (textAsString takeDirectory normalizedDir, Just normalizedDir)
  doesDirectoryExist (Text.unpack dir) >>= \case
    True -> do
      setCurrentDirectory $ Text.unpack dir
      files <- listFilesInDir dir
      let prevDirPosM = findIndex ((== stateCwd) . filePath) files
      let cachedPosM = Map.lookup dir stateListPositionHistory
      let targetFilePosM = targetFilePathM >>= \f -> findIndex ((== f) . filePath) files
      let pos = fromMaybe 0 (targetFilePosM <|> cachedPosM <|> prevDirPosM)
      let list = L.listMoveTo pos $ L.list FocusMain (Vec.fromList files) 1
      pure $ appState {stateFiles = list, stateCwd = dir}
    False -> pure appState

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
cacheDirPosition appState@(AppState {stateListPositionHistory, stateCwd, stateFiles}) =
  appState {stateListPositionHistory = Map.insert stateCwd pos stateListPositionHistory}
  where
    pos = fromMaybe 0 $ L.listSelected stateFiles

filterInvalidSelections :: AppState -> IO AppState
filterInvalidSelections st = do
  selections <- filterM (fileExist . Text.unpack) . Set.elems $ stateFileSelections st
  pure $ st {stateFileSelections = Set.fromList selections}
