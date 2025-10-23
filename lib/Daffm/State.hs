{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}
module Daffm.State where

import Brick (suspendAndResume')
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as L
import Control.Applicative ((<|>))
import Control.Exception (try)
import Control.Monad (filterM, forM)
import qualified Debug.Trace as Debug
import Daffm.Types
import Daffm.Utils (trim)
import Data.List (findIndex, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Zipper.Generic as Zipper
import qualified Data.Vector as Vec
import System.Directory (doesDirectoryExist, doesPathExist, getCurrentDirectory, getHomeDirectory, getSymbolicLinkTarget, listDirectory, makeAbsolute, setCurrentDirectory)
import System.FilePath (joinPath, takeDirectory)
import System.PosixCompat (fileExist)
import qualified System.PosixCompat as Posix

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
      stateKeyMap = configKeymap config,
      stateOpenerScript = configOpener config,
      stateKeySequence = [],
      stateSearchTerm = Nothing,
      stateSearchMatches = Vec.empty,
      stateSearchIndex = 0
    }

toggleSetItem :: (Ord a) => a -> Set.Set a -> Set.Set a
toggleSetItem val set
  | Set.member val set = Set.delete val set
  | otherwise = Set.insert val set

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

stripTrailingSlash :: Text.Text -> Text.Text
stripTrailingSlash path@"/" = path
stripTrailingSlash path = fromMaybe path $ Text.stripSuffix "/" path

textAsString :: (String -> String) -> Text.Text -> Text.Text
textAsString f = Text.pack . f . Text.unpack

loadDirToState :: FilePathText -> AppState -> IO AppState
loadDirToState dir' appState@(AppState {stateCwd, stateListPositionHistory}) = do
  normalizedDir <- (normalizePath . stripTrailingSlash . stripQuotes . trim) dir' >>= withCwdFallback
  stat <- Posix.getFileStatus $ Text.unpack normalizedDir
  let (dir, targetFilePath) =
        if Posix.isDirectory stat
          then (normalizedDir, Nothing)
          else (textAsString takeDirectory normalizedDir, Just normalizedDir)
  doesDirectoryExist (Text.unpack dir) >>= \case
    True -> do
      setCurrentDirectory $ Text.unpack dir
      files <- listFilesInDir dir
      let prevDirPos = findIndex ((== stateCwd) . filePath) files
      let cachedPos = Map.lookup dir stateListPositionHistory
      let targetFilePos = targetFilePath >>= \target -> findIndex ((== target) . filePath) files
      let pos = fromMaybe 0 (targetFilePos <|> cachedPos <|> prevDirPos)
      let list = L.listMoveTo pos $ L.list FocusMain (Vec.fromList files) 1
      pure $ appState {stateFiles = list, stateCwd = dir, stateSearchIndex = 0, stateSearchMatches = Vec.empty}
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
  let either2Maybe :: Either IOError a -> Maybe a
      either2Maybe = either (const Nothing) Just
  linkStat <- either2Maybe <$> try (Posix.getFileStatus path)
  linkTarget <-
    if
      | Posix.isSymbolicLink stat -> Just . Text.pack <$> getSymbolicLinkTarget path
      | otherwise -> pure Nothing
  pure $
    FileInfo
      { filePath = Text.pack path,
        fileName = name,
        fileSize = Posix.fileSize stat,
        fileMode = Posix.fileMode stat,
        fileType = fileTypeFromStatus stat,
        fileLinkType = fileTypeFromStatus <$> linkStat,
        fileLinkTarget = linkTarget
      }

fileSorter :: FileInfo -> FileInfo -> Ordering
fileSorter
  (FileInfo {fileType = Directory, fileName = fa})
  (FileInfo {fileType = Directory, fileName = fb}) =
    compare (Text.toLower fa) (Text.toLower fb)
fileSorter
  (FileInfo {fileType = SymbolicLink, fileLinkType = Just Directory, fileName = fa})
  (FileInfo {fileType = SymbolicLink, fileLinkType = Just Directory, fileName = fb}) =
    compare (Text.toLower fa) (Text.toLower fb)
fileSorter (FileInfo {fileType = Directory}) (FileInfo {fileType = SymbolicLink, fileLinkType = Just Directory}) = LT
fileSorter (FileInfo {fileType = SymbolicLink, fileLinkType = Just Directory}) (FileInfo {fileType = Directory}) = GT
fileSorter (FileInfo {fileType = Directory}) _ = LT
fileSorter _ (FileInfo {fileType = Directory}) = GT
fileSorter (FileInfo {fileLinkType = Just Directory}) _ = LT
fileSorter _ (FileInfo {fileLinkType = Just Directory}) = GT
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
