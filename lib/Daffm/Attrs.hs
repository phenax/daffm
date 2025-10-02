module Daffm.Attrs where

import qualified Brick.AttrMap as A
import Brick.Util (fg)
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V

selectedFileAttr :: A.AttrName
selectedFileAttr = A.attrName "selected-file"

appAttrMap :: A.AttrMap
appAttrMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, fg V.white),
      (selectedFileAttr, fg V.cyan)
    ]
