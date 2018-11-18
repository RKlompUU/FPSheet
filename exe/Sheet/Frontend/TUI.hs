module Sheet.Frontend.TUI where

import Sheet.Backend.Standard
import Sheet.Frontend.Types

import Control.Concurrent.Chan

import Brick.Widgets.Core
import Graphics.Vty
import Graphics.Vty.Attributes
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick
import Brick.Widgets.Edit
import Brick.Types
import Brick.BChan

import Data.List
import Control.Concurrent

import System.Console.Terminal.Size

import qualified Data.Text as T

import System.Exit
import Control.Monad.IO.Class

import Sheet.Frontend.CmdParser

import qualified Data.Map as M

type BrickS = UISheet
type BrickE = CustomEvent -- Custom event type
type BrickN = String -- Custom resource type

runTUI :: IO ()
runTUI = do
  asyncResChan <- Brick.BChan.newBChan 10000
  initialState <- initUISheet asyncResChan
  let app = App { appDraw = drawImpl,
                  appChooseCursor = chooseCursorImpl,
                  appHandleEvent = handleEventImpl,
                  appStartEvent = startEventImpl,
                  appAttrMap = attrMapImpl }
  finalState <- customMain (Graphics.Vty.mkVty Graphics.Vty.defaultConfig)
                (Just asyncResChan)
                app
                initialState
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
       vBox (map (hBox . insertColSeps . map (renderCell s)) cellRows))
       <=>
       renderFooter s]

insertColSeps :: [Widget BrickN] -> [Widget BrickN]
insertColSeps = intercalate [str $ take colSep $ repeat ' '] . map (: [])

renderSelectedCell :: BrickS -> Widget BrickN
renderSelectedCell s =
  let p@(cCursor, rCursor) = sheetCursor s
      cell = M.lookup p (s_cells $ sheetCells s)
      cellRendering = case uiMode s of
                        ModeEdit{cellEditor = editField, cellEditorWidth = editWidth} ->
                          hLimit (editWidth) $ renderEditor (str . flip (++) "." . intercalate "\n") True editField
                        _ -> case cell of
                                        Nothing -> fixedWidthLeftStr (cWidth s) ""
                                        Just c -> case maybe (getText c) id $ getEval c of
                                                    ""   -> fixedWidthLeftStr (cWidth s) ""
                                                    text -> str (take (screenWidth s) text)
  in cellBgAttr s p applyStandout $ translateBy (Location $ sheetCursorPos s) cellRendering

renderFooter :: BrickS -> Widget BrickN
renderFooter s =
  case uiMode s of
    ModeCommand{cmdEditor = editField, cmdEditorWidth = editWidth} ->
      str ":" <+> hLimit (editWidth) (renderEditor (str . flip (++) "." . intercalate "\n") True editField)
    _ -> emptyWidget

editorTextRenderer :: [String] -> Widget BrickN
editorTextRenderer lns =
  let line = intercalate "\n" lns
  in hLimit (length line) $ str line

renderCell :: BrickS -> Pos -> Widget BrickN
renderCell s (col,row) =
  let (cCursor, rCursor) = sheetCursor s
      cell = M.lookup (col,row) (s_cells $ sheetCells s)
  in cellBgAttr s (col,row) id $ case cell of
      Nothing -> fixedWidthLeftStr (cWidth s) ""
      Just c -> case getEval c of
                  Just res -> fixedWidthLeftStr (cWidth s) res
                  Nothing  -> fixedWidthLeftStr (cWidth s) (getText c)

cellBgAttr s (col,row) dflt =
  case M.lookup (col,row) (cellStatus s) of
    Just CellSuccess  -> withAttr greenBg
    Just CellDefined  -> withAttr brightYellowBg
    Just CellUpdating -> withAttr yellowBg
    Just CellFailure  -> withAttr redBg
    Nothing           -> dflt

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
  | col == cCursor = applyStandout $ fixedWidthCenterStr (cWidth s) $ toCol col
  | otherwise      = fixedWidthCenterStr (cWidth s) $ toCol col

renderRowTag :: Pos -> Int -> Widget BrickN
renderRowTag (_,rCursor) row
  | row == rCursor = applyStandout $ str $ show row
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
    uiRows = rows,

    screenWidth = width,
    screenHeight = height
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

delayedCustomEvent :: BChan CustomEvent -> Int -> CustomEvent -> IO ()
delayedCustomEvent chan delayMs custEv = do
  forkIO $ do
    threadDelay (delayMs * 1000)
    writeBChan chan custEv
  return ()

handleEventImpl :: BrickS -> BrickEvent BrickN BrickE -> EventM BrickN (Next BrickS)
handleEventImpl s (AppEvent (EvNewDefinition (BackendJobResponse applyRes))) = do
  cells' <- liftIO $ execStateT applyRes (sheetCells s)
  continue $ s { sheetCells = cells', uiMode = ModeNormal }
handleEventImpl s (AppEvent (EvVisualFeedback c stat)) = do
  case stat of
    CellSuccess -> liftIO $ delayedCustomEvent (custEvChan s) (showCellFeedbackTimeout s) (EvVisualFeedback c CellNoStatus)
    CellDefined ->  liftIO $ delayedCustomEvent (custEvChan s) (showCellFeedbackTimeout s) (EvVisualFeedback c CellNoStatus)
    CellFailure -> liftIO $ delayedCustomEvent (custEvChan s) (showCellFeedbackTimeout s) (EvVisualFeedback c CellNoStatus)
    _ -> return ()
  let cellStatus' = case stat of
                      CellNoStatus -> M.delete (getCellPos c) (cellStatus s)
                      _ -> M.insert (getCellPos c) stat (cellStatus s)
  continue $ s { cellStatus = cellStatus' }
handleEventImpl s (VtyEvent (EvResize width height)) = do
  continue $ uiResize width height s
handleEventImpl s@(UISheet { uiMode = ModeNormal }) ev = do
  case ev of
    VtyEvent (EvKey KEsc []) -> halt s
    VtyEvent (EvKey KEnter []) -> do
      let (col,row) = sheetCursor s
          str = maybe "" getText $ M.lookup (col,row) (s_cells $ sheetCells s)
      continue $ s { uiMode = ModeEdit {cellEditor = editor "Cell editor" (Just 1) str,
                                        cellEditorWidth = max (cWidth s) (length str + 2)} }
    VtyEvent (EvKey (KChar ':') []) -> do
      continue $ s { uiMode = ModeCommand {cmdEditor = editor "Cell editor" (Just 1) "",
                                           cmdEditorWidth = 2} }
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
    _ -> continue s
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
handleEventImpl s@(UISheet { uiMode = m@(ModeCommand{cmdEditor = editField}) }) ev = do
  case ev of
    VtyEvent (EvKey KEsc []) -> continue $ s { uiMode = ModeNormal }
    VtyEvent (EvKey KEnter []) -> do
      let str = intercalate "\n" $ getEditContents editField
      s' <- liftIO $ execCmd str s
      continue $ s' {uiMode = ModeNormal}
    VtyEvent vtEv -> do
      e' <- handleEditorEvent vtEv editField
      continue $ s { uiMode = m {cmdEditor = e', cmdEditorWidth = max (cWidth s) (2 + (length $ intercalate "\n" $ getEditContents e'))} }
    _ -> continue s

startEventImpl :: BrickS -> EventM BrickN BrickS
startEventImpl s = do
  (cols, rows) <- liftIO $ maybe (80,24) (\w -> (width w, height w))
                        <$> size
  return $ uiResize cols rows s

execCmd :: String -> BrickS -> IO BrickS
execCmd str s = do
  case parseCmd str of
    CmdMoveCursor col row -> return $ moveCursor col row s
    CmdInvalid -> return s


attrMapImpl :: BrickS -> AttrMap
attrMapImpl _ = attrMap defAttr [ (blueBg, bg blue),
                                  (redBg, bg red),
                                  (greenBg, bg green),
                                  (yellowBg, bg yellow),
                                  (brightYellowBg, bg brightYellow) ]

withAttrs :: [AttrName] -> Widget BrickN -> Widget BrickN
withAttrs attrs w = foldr withAttr w attrs

blueBg = attrName "blueBg"
redBg = attrName "redBg"
greenBg = attrName "greenBg"
brightYellowBg = attrName "brightYellowBg"
yellowBg = attrName "yellowBg"

applyStandout :: Widget BrickN -> Widget BrickN
applyStandout w =
  withAttr blueBg $ modifyDefAttr (flip withStyle standout) w
