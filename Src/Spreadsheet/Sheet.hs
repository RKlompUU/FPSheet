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


grabUpdatedCells :: Sheet -> Sheet
grabUpdatedCells = Map.filter uFlag

resetUpdateFields :: Sheet -> Sheet
resetUpdateFields = Map.map (\c -> c {uFlag = False})

posSubtr :: Pos -> Pos -> Pos
posSubtr (r1,c1) (r2,c2) = (r1-r2,c1-c2)

posAdd :: Pos -> Pos -> Pos
posAdd (r1,c1) (r2,c2) = (r1+r2,c1+c2)

sliceList :: Int -> Int -> [a] -> [a]
sliceList from to xs = take (to - from + 1) (drop from xs)

subLists :: Int -> [a] -> [[a]]
subLists i xs = let is = [0,i..(length xs - 1)]
                in map (\i' -> sliceList i' (i'+i-1) xs) is

initSheet :: Sheet
initSheet = Map.empty


getSheetCell :: Pos -> Sheet -> Cell
getSheetCell pos cs
  = Map.findWithDefault emptyCell pos cs

emptyCell :: Cell
emptyCell = Cell "" Nothing False

updateCells :: Sheet -> Sheet
updateCells cs
  = case Map.foldrWithKey updateCell (Right cs) cs of
      Left cs' -> updateCells cs'
      Right cs' -> cs'

updateCell :: Pos -> Cell -> Either Sheet Sheet -> Either Sheet Sheet
updateCell _ _ (Left cs)  = Left cs
updateCell p c (Right cs) =
  let lExpr' = parseExpr (Src.Spreadsheet.SheetType.text c) >>= return . nf . toIdInt . expandCellRefs cs
      c'    = c { lExpr = lExpr' }
  in case (Map.lookup p cs >>= \cOld -> return $ lExpr cOld == lExpr') of
     -- Evaluated expr hasn't changed
     Just True -> Right $ Map.insert p c' cs
     -- Evaluated expr has changed, update the entire sheet
     _ -> let cs' = trace ("lExpr' " ++ show p ++ " changed: " ++ show lExpr') Map.insert p (c' {uFlag = True}) cs
          in Left $ cs'


expandCellRefs :: Sheet -> LC String -> LC String
expandCellRefs cs e
  = let refPs = scanCellRefs e
    in addCellRefs (mapMaybe (\p -> (,) <$> pure (cRefPos2Var p) <*> (Map.lookup p cs >>= lExpr >>= return . fromIdInt)) refPs) e

scanCellRefs :: LC v -> [Pos]
scanCellRefs (CVar p)    = [p]
scanCellRefs (Lam _ e)   = scanCellRefs e
scanCellRefs (App e1 e2) = scanCellRefs e1 ++ scanCellRefs e2
scanCellRefs _ = []
