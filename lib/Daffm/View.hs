module Daffm.View where

import Brick.Types (Widget)
import Brick.Widgets.Core (str, vBox, vLimit, withAttr, (<+>))
import qualified Brick.Widgets.List as L
import Daffm.Attrs (selectedFileAttr)
import Daffm.Types (AppState (..), FileInfo (..))
import qualified Data.Vector as Vec

appView :: AppState -> [Widget ()]
appView (AppState {stateFiles, stateCwd}) = [ui]
  where
    ui = vBox [vLimit 1 header, box, vLimit 1 cmdline]
    header = str stateCwd
    cmdline = str "Item " <+> cur <+> str " of " <+> total
    cur = case L.listSelected stateFiles of
      Nothing -> str "-"
      Just i -> str (show (i + 1))
    total = str $ show $ Vec.length $ L.listElements stateFiles
    box = L.renderList fileItemView True stateFiles

fileItemView :: Bool -> FileInfo -> Widget ()
fileItemView sel (FileInfo {fileName, fileSize, fileType}) =
  let wrap w = if sel then withAttr selectedFileAttr w else w
   in wrap (str fileName) <+> str (" : " <> show fileSize <> " | " <> show fileType)
