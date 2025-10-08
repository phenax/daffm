module Daffm.View where

import Brick.Types (Context (availWidth), Size (Fixed), Widget (Widget, render), getContext)
import Brick.Widgets.Core (Padding (Max, Pad), TextWidth (textWidth), emptyWidget, hBox, hLimit, padLeft, padRight, str, txt, vBox, vLimit, withAttr, (<+>))
import Brick.Widgets.Edit (renderEditor)
import qualified Brick.Widgets.List as L
import Daffm.Attrs (directoryAttr, directoryLinkAttr, directorySelectedAttr, fileAttr, fileSelectedAttr, invalidLinkAttr, linkAttr, searchMarchAttr)
import Daffm.Keymap (showKeySequence)
import Daffm.Types (AppState (..), FileInfo (..), FileType (..), FocusTarget (..))
import Data.Int (Int64)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import System.Posix.Types (FileMode)
import qualified System.PosixCompat as Posix
import Text.Printf (printf)

appView :: AppState -> [Widget FocusTarget]
appView appState@(AppState {stateFiles}) = [ui]
  where
    ui = vBox [vLimit 1 header, box, vLimit 1 cmdline]
    header = headerView appState
    cmdline = cmdlineView appState
    box = L.renderListWithIndex (fileItemView appState) True stateFiles

hFixed :: Int -> Widget n -> Widget n
hFixed w = hLimit w . padRight Max

headerView :: AppState -> Widget n
headerView (AppState {stateCwd}) = Widget Fixed Fixed $ do
  c <- getContext
  let width = availWidth c - 2 -- with padding
  render . toWidget . trunc width $ stateCwd
  where
    toWidget = padLeft (Pad 1) . padRight (Pad 1) . txt
    trunc width text
      | width < textWidth text = "…" <> Text.takeEnd (width - 1) text
      | otherwise = Text.takeEnd width text

fileItemView :: AppState -> Int -> Bool -> FileInfo -> Widget FocusTarget
fileItemView appState index sel fileInfo@(FileInfo {filePath, fileSize, fileType, fileMode}) =
  hBox
    [ hFixed 2 fileSelectionView,
      hFixed 10 $ fileModeView fileMode,
      hFixed 6 $ fileTypeView fileType,
      hFixed 7 $ fileSizeView fileSize,
      fileNameView sel fileInfo,
      searchMatchIndicatorView
    ]
  where
    fileSizeView = txt . prettyFileSize . fromIntegral
    fileTypeView = txt . showFileType
    fileModeView = txt . showFileMode
    fileSelectionView = txt $ if Set.member filePath $ stateFileSelections appState then ">" else " "
    searchMatchIndicatorView
      | index `Vec.elem` stateSearchMatches appState = padLeft (Pad 1) $ withAttr searchMarchAttr $ txt "*"
      | otherwise = emptyWidget

showFileType :: FileType -> Text.Text
showFileType Directory = "dir"
showFileType SymbolicLink = "link"
showFileType UnixSocket = "sock"
showFileType NamedPipe = "pipe"
showFileType CharacterDevice = "cdev"
showFileType BlockDevice = "bdev"
showFileType RegularFile = "file"
showFileType UnknownFileType = "?"

showFileMode :: FileMode -> Text.Text
showFileMode mode = permchars
  where
    perm m c = if Posix.intersectFileModes mode m == m then c else '-'
    permchars =
      Text.pack
        [ perm Posix.ownerReadMode 'r',
          perm Posix.ownerWriteMode 'w',
          perm Posix.ownerExecuteMode 'x',
          perm Posix.groupReadMode 'r',
          perm Posix.groupWriteMode 'w',
          perm Posix.groupExecuteMode 'x',
          perm Posix.otherReadMode 'r',
          perm Posix.otherWriteMode 'w',
          perm Posix.otherExecuteMode 'x'
        ]

fileNameView :: Bool -> FileInfo -> Widget FocusTarget
fileNameView True (FileInfo {fileName, fileType = Directory}) = withAttr directorySelectedAttr $ txt $ fileName <> "/"
fileNameView False (FileInfo {fileName, fileType = Directory}) = withAttr directoryAttr $ txt $ fileName <> "/"
fileNameView _ file@(FileInfo {fileType = SymbolicLink}) = symbolicLinkNameView file
fileNameView True (FileInfo {fileName}) = withAttr fileSelectedAttr $ txt fileName
fileNameView False (FileInfo {fileName}) = withAttr fileAttr $ txt fileName

symbolicLinkNameView :: FileInfo -> Widget n
symbolicLinkNameView (FileInfo {fileName, fileLinkTarget, fileLinkType = Just Directory}) =
  withAttr directoryLinkAttr (txt $ fileName <> "/") <+> txt " -> " <+> symTargetView (Just Directory) fileLinkTarget
symbolicLinkNameView (FileInfo {fileName, fileLinkType, fileLinkTarget}) =
  withAttr linkAttr (txt fileName) <+> txt " -> " <+> symTargetView fileLinkType fileLinkTarget

symTargetView :: Maybe FileType -> Maybe Text.Text -> Widget n
symTargetView _ Nothing = withAttr invalidLinkAttr $ txt "<none>"
symTargetView Nothing (Just target) = withAttr invalidLinkAttr $ txt target
symTargetView _ (Just target) = txt target

cmdlineView :: AppState -> Widget FocusTarget
cmdlineView (AppState {stateFocusTarget = FocusCmdline, stateCmdlineEditor}) =
  txt ":" <+> renderEditor (txt . Text.unlines) True stateCmdlineEditor
cmdlineView (AppState {stateFocusTarget = FocusMain, stateFiles, stateFileSelections, stateKeySequence}) =
  hBox
    [ txt ":",
      rightAligned [keysView, padLeft (Pad 2) selectionsCountView, padLeft (Pad 2) posIndicatorView]
    ]
  where
    keysView = txt $ showKeySequence stateKeySequence
    rightAligned = padLeft Max . padRight (Pad 1) . hBox
    posIndicatorView = str $ cur <> "/" <> total
    selectionsCountView =
      if Set.null stateFileSelections
        then emptyWidget
        else str $ show $ Set.size stateFileSelections
    cur = case L.listSelected stateFiles of
      Nothing -> "-"
      Just n -> show (n + 1)
    total = show $ Vec.length $ L.listElements stateFiles

prettyFileSize :: Int64 -> Text.Text
prettyFileSize i
  | i >= 2 ^ (40 :: Int64) = format (i `divBy` (2 ** 40)) <> "T"
  | i >= 2 ^ (30 :: Int64) = format (i `divBy` (2 ** 30)) <> "G"
  | i >= 2 ^ (20 :: Int64) = format (i `divBy` (2 ** 20)) <> "M"
  | i >= 2 ^ (10 :: Int64) = format (i `divBy` (2 ** 10)) <> "K"
  | otherwise = Text.pack $ show i
  where
    format = Text.pack . printf "%0.1f"
    divBy :: Int64 -> Double -> Double
    divBy a b = (fromIntegral a :: Double) / b
