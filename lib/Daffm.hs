module Daffm (app, loadDirInAppState, mkEmptyAppState) where

import qualified Brick.Main as M
import Daffm.Attrs (appAttrMap)
import Daffm.Event (appEvent)
import Daffm.State (loadDirInAppState, mkEmptyAppState)
import Daffm.Types (AppState (..), FocusTarget)
import Daffm.View (appView)

app :: M.App AppState e FocusTarget
app =
  M.App
    { M.appDraw = appView,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = pure (),
      M.appAttrMap = const appAttrMap
    }
