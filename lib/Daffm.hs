module Daffm (initApp, loadDirToState, mkEmptyAppState) where

import qualified Brick.Main as B
import Control.Monad (void)
import Daffm.Attrs (appAttrMap)
import Daffm.Event (appEvent)
import Daffm.State (loadDirToState, mkEmptyAppState)
import Daffm.Types (AppState (..), Configuration, FilePathText, FocusTarget)
import Daffm.View (appView)

app :: B.App AppState e FocusTarget
app =
  B.App
    { B.appDraw = appView,
      B.appChooseCursor = B.showFirstCursor,
      B.appHandleEvent = appEvent,
      B.appStartEvent = pure (),
      B.appAttrMap = const appAttrMap
    }

initApp :: FilePathText -> Configuration -> IO ()
initApp dir config = do
  initialState <- loadDirToState dir $ mkEmptyAppState config
  void $ B.defaultMain app initialState
