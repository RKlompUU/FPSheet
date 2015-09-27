module Src.Spreadsheet.Sheet
      ( module Src.Spreadsheet.Sheet
      , module Src.Spreadsheet.SheetType
      ) where

import Src.Spreadsheet.SheetType

import Data.Maybe

import Control.Monad
import Control.Concurrent.STM

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Aeson                  as JSON

import Debug.Trace

import Src.Lambda.Lambda

readonly :: Attr Element Bool
readonly = fromJQueryProp "readonly" (== JSON.Bool True) JSON.Bool


-- Returns the offset of a position towards a box, if the position is inside the box Nothing is returned
isInBox :: Pos -> (Pos,Pos) -> Maybe Pos
isInBox (r,c) ((rL, cL), (rH, cH))
  = let rOffset = if r < rL
                    then r - rL
                    else if r > rH
                      then r - rH
                      else 0
        cOffset = if c < cL
                    then c - cL
                    else if c > cH
                      then c - cH
                      else 0
    in if rOffset == 0 && cOffset == 0
        then Nothing
        else Just (rOffset,cOffset)


grabUpdatedCells :: Map Pos Cell -> Map Pos Cell
grabUpdatedCells = Map.filter uFlag

resetUpdateFields :: Map Pos Cell -> Map Pos Cell
resetUpdateFields = Map.map (\c -> c {uFlag = False})

posSubtr :: Pos -> Pos -> Pos
posSubtr (r1,c1) (r2,c2) = (r1-r2,c1-c2)

posAdd :: Pos -> Pos -> Pos
posAdd (r1,c1) (r2,c2) = (r1+r2,c1+c2)

grabShell :: (Pos, (Element, Element)) -> Element
grabShell = fst . snd
grabCell :: (Pos, (Element, Element)) -> Element
grabCell = snd . snd
grabPos :: (Pos, (Element, Element)) -> Pos
grabPos = fst

getSheet :: TVar Sheet -> UI Sheet
getSheet ctxSh = liftIO $ atomically $ readTVar ctxSh

sliceList :: Int -> Int -> [a] -> [a]
sliceList from to xs = take (to - from + 1) (drop from xs)

subLists :: Int -> [a] -> [[a]]
subLists i xs = let is = [0,i..(length xs - 1)]
                in map (\i' -> sliceList i' (i'+i-1) xs) is

initSheet :: UI Sheet
initSheet = do
  let cols = 7
  let rows = 12
--  cells <- replicateM (rows*cols) (UI.input # set readonly True # set UI.bgcolor "#00000")
  cells <- replicateM (rows*cols) (UI.input # set UI.size "7")
  shells <- replicateM (rows*cols) UI.button
  rowNrs <- replicateM rows (UI.body # set UI.text "0")
  colNrs <- replicateM cols (UI.body # set UI.text "0")
  let shelledCells = zip shells cells
  let cells' = map (\(rI,r) -> zip [(rI,cI) | cI <- [0..cols]] r)
             $ zip [0..rows]
             $ subLists cols shelledCells
  return $ Sheet (0, 0) Map.empty (0, 0) cells' colNrs rowNrs

focusSheetInShell :: TVar Sheet -> UI ()
focusSheetInShell ctxSh
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  let focusIn = sheetFocus sh
  UI.setFocus (grabShell (sheetIns sh !! fst focusIn !! snd focusIn))

moveFocus :: TVar Sheet -> Pos -> UI ()
moveFocus ctxSh dPos
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  let (fRow,fCol) = dPos `posAdd` sheetFocus sh
      rows = length (sheetIns sh) - 1
      cols = length (head $ sheetIns sh) - 1
  case isInBox (fRow,fCol) ((0,0),(rows,cols)) of
    Just offset -> scrollSheet ctxSh offset
    Nothing -> liftIO (atomically $ writeTVar ctxSh (sh {sheetFocus = dPos `posAdd` sheetFocus sh}))
               >> focusSheetInShell ctxSh

getAbsoluteCPos :: TVar Sheet -> Pos -> UI Pos
getAbsoluteCPos ctxSh relativePos
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  return $ relativePos `posAdd` sheetOffset sh

-- Unsafe operation, will crash if an invalid position is given
getSheetIn :: Pos -> Sheet -> Element
getSheetIn (r,c) sh
  = grabCell $ sheetIns sh !! r !! c

getSheetCell :: Pos -> Sheet -> Cell
getSheetCell pos sh
  = Map.findWithDefault emptyCell pos (sheetCells sh)

emptyCell :: Cell
emptyCell = Cell "" Nothing False

cells2Ins :: TVar Sheet -> UI ()
cells2Ins ctxSh
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  mapM_ (\(p, (_,elm)) -> cell2In (sheetCells sh) (p `posAdd` sheetOffset sh) elm) (concat $ sheetIns sh)
  --liftIO $ atomically $ writeTVar ctxSh (sh {sheetIns = })

cell2In :: Map Pos Cell -> Pos -> Element -> UI ()
cell2In cs pos elm
  = do
  let (Cell text _ _) = Map.findWithDefault emptyCell pos cs
  oldVal <- get UI.value elm
  unless (text == oldVal) $ element elm # set UI.value text >> return ()

offsetSheet :: TVar Sheet -> Pos -> UI ()
offsetSheet ctxSh pos
  = do
  sh <- getSheet ctxSh
  liftIO $ atomically $ writeTVar ctxSh (sh {sheetOffset = pos})
  mapM_ (\(elm,rowNr) -> element elm # set UI.text (show rowNr)) (zip (sheetRowNs sh) [fst pos..])
  mapM_ (\(elm,colNr) -> element elm # set UI.text (show colNr)) (zip (sheetColNs sh) [snd pos..])
  cells2Ins ctxSh
  return ()

scrollSheet :: TVar Sheet -> Pos -> UI ()
scrollSheet ctxSh dPos
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  offsetSheet ctxSh (dPos `posAdd` sheetOffset sh)

updateCells :: Map Pos Cell -> Map Pos Cell
updateCells cs
  = Map.foldrWithKey updateCell cs cs

updateCell :: Pos -> Cell -> Map Pos Cell -> Map Pos Cell
updateCell p c cs
  = let lExpr' = parseExpr (Src.Spreadsheet.SheetType.text c) >>= return . nf . toIdInt . (\v -> trace ("Test: " ++ show (expandCellRefs cs v)) (expandCellRefs cs v))
        c'    = c { lExpr = lExpr' }
    in case (Map.lookup p cs >>= \cOld -> return $ trace ("old: " ++ show (lExpr cOld) ++ ",, new: " ++ show lExpr') lExpr cOld == lExpr') of
        -- Evaluated expr hasn't changed
        Just True -> trace ("No change") Map.insert p c' cs
        -- Evaluated expr has changed, update the entire sheet
        _ -> let cs' = trace ("changed lExpr': " ++ show lExpr') Map.insert p (c' {uFlag = True})  cs
             in updateCells cs'

cellMod :: String -> Pos -> Sheet -> Sheet
cellMod cCnt cPos sh
  = let mC     = Map.lookup cPos (sheetCells sh)
        lExpr' = parseExpr cCnt >>= return . nf . toIdInt
        c'     = Cell cCnt lExpr' False
    in sh { sheetCells = updateCell cPos c' (sheetCells sh) }


expandCellRefs :: Map Pos Cell -> LC String -> LC String
expandCellRefs cs e
  = let refPs = scanCellRefs e
    in trace ("cellRefs: " ++ show refPs) addCellRefs (mapMaybe (\p -> (,) <$> pure (cRefPos2Var p) <*> (Map.lookup p cs >>= lExpr >>= return . fromIdInt)) refPs) e

scanCellRefs :: LC v -> [Pos]
scanCellRefs (CVar p)    = [p]
scanCellRefs (Lam _ e)   = scanCellRefs e
scanCellRefs (App e1 e2) = scanCellRefs e1 ++ scanCellRefs e2
scanCellRefs _ = []
