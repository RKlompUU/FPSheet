module Sheet.Frontend.TUI where

import Sheet.Backend.Standard
import Sheet.Frontend.Types

import Brick.Widgets.Core
import Graphics.Vty
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
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
  let (rowOffset, colOffset) = sheetOffset s
      colsNum = 10
      rowsNum = 20

      cols = [colOffset..colOffset + colsNum]
      rows = [rowOffset..rowOffset + rowsNum]

      colsHeader = hBox $ map (renderColTag (sheetCursor s)) cols
      rowsHeader = vBox $ map (renderRowTag (sheetCursor s)) rows

      cells = s_cells $ sheetCells s
      cellRows = [[M.lookup (c,r) cells | c <- cols] | r <- rows]

      rawSheet = M.toList (s_cells $ sheetCells s)
  in [(str " " <=> str " " <=> (rowsHeader <+> vBorder)) <+>
      (colsHeader <=> hBorder <=>
       vBox (map (hBox . map renderCell) cellRows))]

renderCell :: Maybe C -> Widget BrickN
renderCell Nothing = str " "
renderCell (Just c) =
  case getEval c of
    Just res -> str res
    Nothing  -> str (getText c)

renderColTag :: Pos -> Int -> Widget BrickN
renderColTag (cCursor,_) col
  | col == cCursor = withAttr blueBg $ hCenter $ str $ toCol col
  | otherwise      = hCenter $ str $ toCol col

renderRowTag :: Pos -> Int -> Widget BrickN
renderRowTag (_,rCursor) row
  | row == rCursor = withAttr blueBg $ str $ show row
  | otherwise      = str $ show row

chooseCursorImpl :: BrickS -> [CursorLocation BrickN] -> Maybe (CursorLocation BrickN)
chooseCursorImpl _ _ = Nothing

handleEventImpl :: BrickS -> BrickEvent BrickN BrickE -> EventM BrickN (Next BrickS)
handleEventImpl s ev =
  case ev of
    VtyEvent (EvKey KEsc []) -> halt s
    VtyEvent (EvKey KRight []) -> do
      let (cCursor, rCursor) = sheetCursor s
      continue (s {sheetCursor = (cCursor+1, rCursor)})
    VtyEvent (EvKey KLeft []) -> do
      let (cCursor, rCursor) = sheetCursor s
      continue (s {sheetCursor = (max 1 (cCursor-1), rCursor)})
    VtyEvent (EvKey KUp []) -> do
      let (cCursor, rCursor) = sheetCursor s
      continue (s {sheetCursor = (cCursor, max 1 (rCursor-1))})
    VtyEvent (EvKey KDown []) -> do
      let (cCursor, rCursor) = sheetCursor s
      continue (s {sheetCursor = (cCursor, rCursor+1)})
    _ -> do
      cells' <- liftIO $ do
        flip execStateT (sheetCells s) $ do
          let p = (1,1)
              p0 = (1,5)
          getCell p >>= setText "8 - 3" >>= evalCell
          getCell p0 >>= setText "5 * 3" >>= evalCell
      continue (s {sheetCells = cells'})

startEventImpl :: BrickS -> EventM BrickN BrickS
startEventImpl s = return s

attrMapImpl :: BrickS -> AttrMap
attrMapImpl _ = attrMap defAttr [ (blueBg, bg blue) ]

blueBg = attrName "blueBg"
