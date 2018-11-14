module Sheet.Frontend.TUI where

import Sheet.Backend.Standard
import Sheet.Frontend.Types

import Control.Concurrent.Chan

import Brick.Widgets.Core
import Graphics.Vty
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick
import Brick.Widgets.Edit
import Brick.Types

import Data.List


import System.Console.Terminal.Size

import qualified Data.Text as T

import System.Exit
import Control.Monad.IO.Class

import qualified Data.Map as M

type BrickS = UISheet
type BrickE = () -- Custom event type
type BrickN = String -- Custom resource type

runTUI :: IO ()
runTUI = do
  initialState <- initUISheet
  let app = App { appDraw = drawImpl,
                  appChooseCursor = chooseCursorImpl,
                  appHandleEvent = handleEventImpl,
                  appStartEvent = startEventImpl,
                  appAttrMap = attrMapImpl }
  finalState <- defaultMain app initialState
  return ()

drawImpl :: BrickS -> [Widget BrickN]
drawImpl s =
  let (colOffset, rowOffset) = sheetOffset s
      colsNum = uiCols s
      rowsNum = uiRows s

      cols = [colOffset..colOffset + colsNum]
      rows = [rowOffset..rowOffset + rowsNum]

      colsHeader = hBox
                 $ insertColSeps
                 $ map (renderColTag s (sheetCursor s)) cols
      rowsHeader = vBox $ map (renderRowTag (sheetCursor s)) rows

      cells = s_cells $ sheetCells s
      cellRows = [[(c,r) | c <- cols] | r <- rows]
  in renderSelectedCell s :
     [(str " " <=>
       fixedWidthRightStr (headerWidth s) "," <=>
       (rowsHeader <+> vBorder)) <+>
      (colsHeader <=>
       hBorder <=>
       vBox (map (hBox . insertColSeps . map (renderCell s)) cellRows))]

insertColSeps :: [Widget BrickN] -> [Widget BrickN]
insertColSeps = intercalate [str $ take colSep $ repeat ' '] . map (: [])

renderSelectedCell :: BrickS -> Widget BrickN
renderSelectedCell s =
  let p@(cCursor, rCursor) = sheetCursor s
      cell = M.lookup p (s_cells $ sheetCells s)
      cellRendering = case uiMode s of
                        ModeNormal -> case cell of
                                        Nothing -> fixedWidthLeftStr (cWidth s) ""
                                        Just c -> case maybe (getText c) id $ getEval c of
                                                    ""   -> fixedWidthLeftStr (cWidth s) ""
                                                    text -> str text

                        ModeEdit{cellEditor = editField, cellEditorWidth = editWidth} ->
                          hLimit (editWidth) $ renderEditor (str . flip (++) "." . intercalate "\n") True editField
  in withAttr blueBg $ translateBy (Location $ sheetCursorPos s) cellRendering

editorTextRenderer :: [String] -> Widget BrickN
editorTextRenderer lns =
  let line = intercalate "\n" lns
  in hLimit (length line) $ str line

renderCell :: BrickS -> Pos -> Widget BrickN
renderCell s (col,row) =
  let (cCursor, rCursor) = sheetCursor s
      cell = M.lookup (col,row) (s_cells $ sheetCells s)
  in case cell of
      Nothing -> fixedWidthLeftStr (cWidth s) ""
      Just c -> case getEval c of
                  Just res -> fixedWidthLeftStr (cWidth s) res
                  Nothing  -> fixedWidthLeftStr (cWidth s) (getText c)

fixedWidthLeftStr :: Int -> String -> Widget BrickN
fixedWidthLeftStr width str =
  txt $ T.justifyLeft width ' ' $ T.pack $ take width str

fixedWidthCenterStr :: Int -> String -> Widget BrickN
fixedWidthCenterStr width str =
  txt $ T.center width ' ' $ T.pack $ take width str

fixedWidthRightStr :: Int -> String -> Widget BrickN
fixedWidthRightStr width str =
  txt $ T.justifyRight width ' ' $ T.pack $ take width str

renderColTag :: BrickS -> Pos -> Int -> Widget BrickN
renderColTag s (cCursor,_) col
  | col == cCursor = withAttr blueBg $ fixedWidthCenterStr (cWidth s) $ toCol col
  | otherwise      = fixedWidthCenterStr (cWidth s) $ toCol col

renderRowTag :: Pos -> Int -> Widget BrickN
renderRowTag (_,rCursor) row
  | row == rCursor = withAttr blueBg $ str $ show row
  | otherwise      = str $ show row

chooseCursorImpl :: BrickS -> [CursorLocation BrickN] -> Maybe (CursorLocation BrickN)
chooseCursorImpl _ [] = Nothing
chooseCursorImpl _ cs = Just $ head cs

sheetCursorPos :: UISheet -> Pos
sheetCursorPos s =
  let (cCursor, rCursor) = sheetCursor s
      (cOffset, rOffset) = sheetOffset s
      x = (cCursor - cOffset) * (cWidth s + colSep) + headerWidth s
      y = (rCursor - rOffset) + headerHeight s
  in (x,y)

uiResize :: Int -> Int -> UISheet -> UISheet
uiResize width height s =
  let rows = height - headerHeight s - 2
      cols = ((width - (headerWidth (s {uiRows = rows})) - 1) `div` (cWidth s + colSep)) - 1
  in s {
    uiCols = cols,
    uiRows = rows
  }

headerWidth :: UISheet -> Int
headerWidth s =
  let (_,rOffset) = sheetOffset s
  in (length $ show (rOffset + uiRows s)) + 1

headerHeight :: UISheet -> Int
headerHeight _ = 2

colSep :: Int
colSep = 1

moveCursor :: Int -> Int -> BrickS -> BrickS
moveCursor toCol toRow s =
  let toCol' = max 1 toCol
      toRow' = max 1 toRow
      (offsetCol, offsetRow) = sheetOffset s
      offsetCol' = if toCol' < offsetCol
                    then toCol'
                    else if toCol' > offsetCol + uiCols s
                          then toCol' - uiCols s
                          else offsetCol
      offsetRow' = if toRow' < offsetRow
                    then toRow'
                    else if toRow' > offsetRow + uiRows s
                          then toRow' - uiRows s
                          else offsetRow
  in s { sheetCursor = (toCol', toRow'), sheetOffset = (offsetCol', offsetRow') }

handleEventImpl :: BrickS -> BrickEvent BrickN BrickE -> EventM BrickN (Next BrickS)
handleEventImpl s (VtyEvent (EvResize width height)) = do
  continue $ uiResize width height s
handleEventImpl s@(UISheet { uiMode = ModeNormal }) ev = do
  case ev of
    VtyEvent (EvKey KEsc []) -> halt s
    VtyEvent (EvKey KEnter []) -> do
      let (col,row) = sheetCursor s
          str = maybe "" getText $ M.lookup (col,row) (s_cells $ sheetCells s)
      continue $ s { uiMode = ModeEdit {cellEditor = editor "Cell editor" (Just 1) str, cellEditorWidth = max (cWidth s) (length str + 2)} }
    VtyEvent (EvKey KRight []) -> do
      let (cCursor, rCursor) = sheetCursor s
      continue $ moveCursor (cCursor + 1) rCursor s
    VtyEvent (EvKey KLeft []) -> do
      let (cCursor, rCursor) = sheetCursor s
      continue $ moveCursor (cCursor-1) rCursor s
    VtyEvent (EvKey KUp []) -> do
      let (cCursor, rCursor) = sheetCursor s
      continue $ moveCursor cCursor (rCursor-1) s
    VtyEvent (EvKey KDown []) -> do
      let (cCursor, rCursor) = sheetCursor s
      continue $ moveCursor cCursor (rCursor + 1) s
    _ -> do
      cells' <- liftIO $ do
        flip execStateT (sheetCells s) $ do
          let p = (1,1)
              p0 = (3,4)
          getCell p >>= setText "8 - 3" >>= evalCell
          getCell p0 >>= setText "5 * 3" >>= evalCell
      continue (s {sheetCells = cells'})
handleEventImpl s@(UISheet { uiMode = m@(ModeEdit{cellEditor = editField}) }) ev = do
  case ev of
    VtyEvent (EvKey KEsc []) -> continue $ s { uiMode = ModeNormal }
    VtyEvent (EvKey KEnter []) -> do
      let str = intercalate "\n" $ getEditContents editField
      cells' <- liftIO $ do
        flip execStateT (sheetCells s) $ do
          let p = sheetCursor s
          getCell p >>= setText str >>= evalCell
      continue $ s { sheetCells = cells', uiMode = ModeNormal }
    VtyEvent vtEv -> do
      e' <- handleEditorEvent vtEv editField
      continue $ s { uiMode = m {cellEditor = e', cellEditorWidth = max (cWidth s) (2 + (length $ intercalate "\n" $ getEditContents e'))} }
    _ -> continue s

startEventImpl :: BrickS -> EventM BrickN BrickS
startEventImpl s = do
  (cols, rows) <- liftIO $ maybe (80,24) (\w -> (width w, height w))
                        <$> size
  return $ uiResize cols rows s

attrMapImpl :: BrickS -> AttrMap
attrMapImpl _ = attrMap defAttr [ (blueBg, bg blue) ]

withAttrs :: [AttrName] -> Widget BrickN -> Widget BrickN
withAttrs attrs w = foldr withAttr w attrs

blueBg = attrName "blueBg"
dialog = attrName "dialog"
