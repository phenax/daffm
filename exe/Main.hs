module Main where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (fg)
import Brick.Widgets.Core
  ( str,
    vBox,
    vLimit,
    withAttr,
    (<+>),
  )
import qualified Brick.Widgets.List as L
import Control.Monad (void)
import Control.Monad.State (gets, modify)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

drawUI :: (Show a) => L.List () a -> [Widget ()]
drawUI l = [ui]
  where
    ui =
      vBox
        [ box,
          vLimit 1 label
        ]
    label = str "Item " <+> cur <+> str " of " <+> total
    cur = case L.listSelected l of
      Nothing -> str "-"
      Just i -> str (show (i + 1))
    total = str $ show $ Vec.length $ L.listElements l
    box =
      L.renderList listDrawElement True l

appEvent :: T.BrickEvent () e -> T.EventM () (L.List () Char) ()
appEvent (T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar '+') [] -> do
      els <- gets L.listElements
      let el = nextElement els
          pos = Vec.length els
      modify $ L.listInsert pos el
    V.EvKey (V.KChar '-') [] -> do
      sel <- gets L.listSelected
      case sel of
        Nothing -> pure ()
        Just i -> modify $ L.listRemove i
    V.EvKey V.KEsc [] -> M.halt
    ev -> L.handleListEvent ev
  where
    nextElement :: Vec.Vector Char -> Char
    nextElement v = fromMaybe '?' $ Vec.find (`Vec.notElem` v) (Vec.fromList ['a' .. 'z'])
appEvent _ = pure ()

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s =
        if sel
          then withAttr customAttr (str $ "<" <> s <> ">")
          else str s
   in str "Item " <+> selStr (show a)

initialState :: L.List () Char
initialState = L.list () (Vec.fromList ['a', 'b', 'c']) 1

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, fg V.blue),
      (L.listSelectedAttr, fg V.white),
      (customAttr, fg V.cyan)
    ]

theApp :: M.App (L.List () Char) e ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = pure (),
      M.appAttrMap = const theMap
    }

main :: IO ()
main = void $ M.defaultMain theApp initialState
