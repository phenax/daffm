module Daffm.View where

import Brick.Types (Widget)
import Brick.Widgets.Core (Padding (Max, Pad), TextWidth (textWidth), hBox, hLimit, padLeft, padRight, str, vBox, vLimit, withAttr, (<+>))
import Brick.Widgets.Edit (renderEditor)
import qualified Brick.Widgets.List as L
import Daffm.Attrs (directoryAttr, directorySelectedAttr, fileAttr, fileSelectedAttr)
import Daffm.Types (AppState (..), FileInfo (..), FileType (..), FocusTarget (..))
import Data.Int (Int64)
import qualified Data.Vector as Vec
import Text.Printf (printf)

appView :: AppState -> [Widget FocusTarget]
appView appState@(AppState {stateFiles, stateCwd}) = [ui]
  where
    ui :: Widget FocusTarget
    ui = vBox [vLimit 1 header, box, vLimit 1 cmdline]
    header = str stateCwd
    cmdline = cmdlineView appState
    box :: Widget FocusTarget
    box = L.renderList fileItemView True stateFiles

fixedColumnsStr :: Int -> Widget n -> Widget n
fixedColumnsStr w s = hLimit w $ padRight Max s

fileItemView :: Bool -> FileInfo -> Widget FocusTarget
fileItemView sel fileInfo@(FileInfo {fileSize, fileType}) =
  hBox
    [ fixedColumnsStr 5 (fileTypeView fileType),
      fixedColumnsStr 7 (fileSizeView fileSize),
      fileNameView sel fileInfo
    ]
  where
    fileSizeView = str . prettyFileSize . fromIntegral
    fileTypeView = str . showFileType
    showFileType Directory = "dir"
    showFileType SymbolicLink = "link"
    showFileType UnixSocket = "sock"
    showFileType NamedPipe = "pipe"
    showFileType CharacterDevice = "cdev"
    showFileType BlockDevice = "bdev"
    showFileType RegularFile = "file"

fileNameView :: Bool -> FileInfo -> Widget FocusTarget
fileNameView True (FileInfo {fileName, fileType = Directory}) = withAttr directorySelectedAttr $ str $ fileName <> "/"
fileNameView False (FileInfo {fileName, fileType = Directory}) = withAttr directoryAttr $ str $ fileName <> "/"
fileNameView True (FileInfo {fileName}) = withAttr fileSelectedAttr $ str fileName
fileNameView False (FileInfo {fileName}) = withAttr fileAttr $ str fileName

cmdlineView :: AppState -> Widget FocusTarget
cmdlineView (AppState {stateFocusTarget = FocusCmdline, stateCmdlineEditor}) =
  str ":" <+> renderEditor (str . unlines) True stateCmdlineEditor
cmdlineView (AppState {stateFiles}) =
  hBox [str ":", padLeft Max $ padRight (Pad 1) posIndicator]
  where
    posIndicator = str $ cur <> "/" <> total
    cur = case L.listSelected stateFiles of
      Nothing -> "-"
      Just n -> show (n + 1)
    total = show $ Vec.length $ L.listElements stateFiles

prettyFileSize :: Int64 -> String
prettyFileSize i
  | i >= 2 ^ (40 :: Int64) = format (i `divBy` (2 ** 40)) <> "T"
  | i >= 2 ^ (30 :: Int64) = format (i `divBy` (2 ** 30)) <> "G"
  | i >= 2 ^ (20 :: Int64) = format (i `divBy` (2 ** 20)) <> "M"
  | i >= 2 ^ (10 :: Int64) = format (i `divBy` (2 ** 10)) <> "K"
  | otherwise = show i
  where
    format = printf "%0.1f"
    divBy :: Int64 -> Double -> Double
    divBy a b = (fromIntegral a :: Double) / b
