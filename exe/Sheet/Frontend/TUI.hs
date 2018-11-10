module Sheet.Frontend.TUI where

import Sheet.Backend.Standard
import Sheet.Frontend.Types

import Brick.Widgets.Core
import Graphics.Vty
import Brick.Util
import Brick

import System.Exit
import Control.Monad.IO.Class

import qualified Data.Map as M

type BrickS = UISheet
type BrickE = () -- Custom event type
type BrickN = () -- Custom resource type

runTUI :: BrickS -> IO ()
runTUI initialState = do
  let app = App { appDraw = drawImpl,
                  appChooseCursor = chooseCursorImpl,
                  appHandleEvent = handleEventImpl,
                  appStartEvent = startEventImpl,
                  appAttrMap = attrMapImpl }
  finalState <- defaultMain app initialState
  return ()

drawImpl :: BrickS -> [Widget BrickN]
drawImpl s =
  let rawSheet = M.toList (s_cells $ sheetCells s)
  in [vBox $ map (str . show) rawSheet ]

chooseCursorImpl :: BrickS -> [CursorLocation BrickN] -> Maybe (CursorLocation BrickN)
chooseCursorImpl _ _ = Nothing

handleEventImpl :: BrickS -> BrickEvent BrickN BrickE -> EventM BrickN (Next BrickS)
handleEventImpl s ev =
  case ev of
    VtyEvent (EvKey KEsc []) -> halt s
    _ -> do
      cells' <- liftIO $ do
        flip execStateT (sheetCells s) $ do
          let p = (1,1)
          getCell p >>= setText "8 - 3" >>= evalCell
      continue (s {sheetCells = cells'})

startEventImpl :: BrickS -> EventM BrickN BrickS
startEventImpl s = return s

attrMapImpl :: BrickS -> AttrMap
attrMapImpl _ = attrMap defAttr [ (blueBg, bg blue) ]

blueBg = attrName "blueBg"
