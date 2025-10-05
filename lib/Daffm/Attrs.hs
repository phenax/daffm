module Daffm.Attrs where

import qualified Brick.AttrMap as A
import Brick.Util (bg, fg)
import Brick.Widgets.List (listAttr, listSelectedAttr)
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V

fileAttr :: A.AttrName
fileAttr = listAttr <> A.attrName "file"

fileSelectedAttr :: A.AttrName
fileSelectedAttr = listSelectedAttr <> fileAttr

directoryAttr :: A.AttrName
directoryAttr = listAttr <> A.attrName "directory"

linkAttr :: A.AttrName
linkAttr = listAttr <> A.attrName "link"

invalidLinkAttr :: A.AttrName
invalidLinkAttr = linkAttr <> A.attrName "invalid"

directoryLinkAttr :: A.AttrName
directoryLinkAttr = linkAttr <> A.attrName "directory"

directorySelectedAttr :: A.AttrName
directorySelectedAttr = listSelectedAttr <> directoryAttr

searchMarchAttr :: A.AttrName
searchMarchAttr = listAttr <> A.attrName "match-indicator"

appAttrMap :: A.AttrMap
appAttrMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, fg V.white),
      (listSelectedAttr, bg V.black),
      (directoryAttr, fg V.brightCyan),
      (directorySelectedAttr, fg V.brightCyan),
      (directoryLinkAttr, fg V.green),
      (linkAttr, fg V.brightWhite),
      (invalidLinkAttr, fg V.red),
      (fileAttr, fg V.white),
      (fileSelectedAttr, fg V.white),
      (searchMarchAttr, fg V.magenta)
    ]
