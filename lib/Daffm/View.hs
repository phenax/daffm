module Daffm.View where

import Brick.Types (Widget)
import Brick.Widgets.Core (Padding (Max, Pad), hBox, hLimit, padLeft, padRight, str, vBox, vLimit, withAttr, (<+>))
import Brick.Widgets.Edit (renderEditor)
import qualified Brick.Widgets.List as L
import Daffm.Attrs (directoryAttr, directorySelectedAttr, fileAttr, fileSelectedAttr)
import Daffm.Types (AppState (..), FileInfo (..), FileType (..), FocusTarget (..))
import Data.Int (Int64)
import qualified Data.Vector as Vec
import Text.Printf (printf)

appView :: AppState -> [Widget FocusTarget]
appView appState@(AppState {stateFiles}) = [ui]
  where
    ui = vBox [vLimit 1 header, box, vLimit 1 cmdline]
    header = headerView appState
    cmdline = cmdlineView appState
    box = L.renderList fileItemView True stateFiles

hFixed :: Int -> Widget n -> Widget n
hFixed w = hLimit w . padRight Max

headerView :: AppState -> Widget n
headerView (AppState {stateCwd}) = str stateCwd

fileItemView :: Bool -> FileInfo -> Widget FocusTarget
fileItemView sel fileInfo@(FileInfo {fileSize, fileType}) =
  hBox
    [ hFixed 6 (fileTypeView fileType),
      hFixed 7 (fileSizeView fileSize),
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
    showFileType UnknownFileType = "?"

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
