{-|
Module      : SpreedSheet.Sheet
Description : An experimental application of the spreadsheet API
Stability   : experimental
-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Spreadsheet.Sheet
      ( module Spreadsheet.Sheet
      , module Spreadsheet.SheetType
      , module Lambda.Lambda
      ) where

import Spreadsheet.SheetType

import Data.Maybe

import Control.Monad
import Control.Concurrent.STM

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Aeson                  as JSON

import API.SheetAbstr

import Debug.Trace

import Lambda.Lambda


type CellCntTy = LExpr String

instance Spreadsheet (Sheet CellCntTy) (CellT CellCntTy) CellCntTy String where
  updateEvals s =
    case Map.foldrWithKey updateEval (Right s) s of
      Left s' -> updateEvals s'
      Right s' -> s'
  getCell s p = Map.lookup p s
  setCell s p c = Map.insert p c s

updateEval :: Pos -> CellT CellCntTy -> Either (Sheet CellCntTy) (Sheet CellCntTy) -> Either (Sheet CellCntTy) (Sheet CellCntTy)
updateEval p c (Right s) =
  let c' = parseCell c
      globVars = maybe []
                       (mapMaybe (\p -> getCell s p >>= getEval >>= \e -> return (cRefPos2Var p, e)) . scanCellRefs)
                       (getEval c' >>= return . lExpr_)
      c'' = evalCell
          $ setGlobalVars c' globVars
      oldEval = getEval c >>= return . lExpr_
      newEval = getEval c'' >>= return . lExpr_
  in if (oldEval == newEval)
       then Right s
       else let s' = setCell s p (c'' {uFlag = True})
                in Left s'
updateEval _ _ (Left s) = Left s


instance Cell (CellT CellCntTy) CellCntTy String where
  evalCell c@CellT {lExpr = maybeE} =
    case maybeE of
      Just e  -> c {lExpr = Just $ (evalExpr e)}
      Nothing -> c
  parseCell c@CellT {Spreadsheet.SheetType.text = code} =
    c {lExpr = flip LExpr Map.empty <$> parseExpr code}
  getEval = lExpr
  getText = Spreadsheet.SheetType.text
  setGlobalVars c@CellT {lExpr = maybeE} defs =
    case maybeE of
      Just e  -> c {lExpr = Just $ foldr (\(v,def) e' -> addGlobalVar e' v def) (cleanGlobalVars e) defs}
      Nothing -> c

readonly :: Attr Element Bool
readonly = fromJQueryProp "readonly" (== JSON.Bool True) JSON.Bool

-- | 'isInBox' if the position is inside the box 'Nothing' is returned.
-- Otherwise, it returns the offset of the position towards the box.
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

-- | 'grabUpdatedCells' filters out all cells that have not changed.
grabUpdatedCells :: (Var v, Expr e v) => Sheet e -> Sheet e
grabUpdatedCells = Map.filter uFlag

-- | 'resetUpdateFields' removes the update flags of all cells.
resetUpdateFields :: (Var v, Expr e v) => Sheet e -> Sheet e
resetUpdateFields = Map.map (\c -> c {uFlag = False})

-- | Subtraction on 'Pos' variables.
posSubtr :: Pos -> Pos -> Pos
posSubtr (r1,c1) (r2,c2) = (r1-r2,c1-c2)

-- | Addition on 'Pos' variables.
posAdd :: Pos -> Pos -> Pos
posAdd (r1,c1) (r2,c2) = (r1+r2,c1+c2)

-- | 'sliceList' grabs a part of list 'xs' that ranges from index 'from' to
-- index 'to'.
sliceList :: Int -> Int -> [a] -> [a]
sliceList from to xs = take (to - from + 1) (drop from xs)

-- | 'subLists' slices a list 'xs', where each slice has a length of at most
-- 'i'.
subLists :: Int -> [a] -> [[a]]
subLists i xs = let is = [0,i..(length xs - 1)]
                in map (\i' -> sliceList i' (i'+i-1) xs) is

initSheet :: (Var v, Expr e v) => Sheet e
initSheet = Map.empty

-- | Helper function to conveniently obtain a 'CellT e' from the 'Sheet e'.
getSheetCell :: (Var v, Expr e v) => Pos -> Sheet e -> CellT e
getSheetCell pos cs
  = Map.findWithDefault emptyCell pos cs

emptyCell :: (Var v, Expr e v) => CellT e
emptyCell = CellT "" Nothing False

-- | 'scanCellRefs' obtains all references that are present in an
-- expression. This is for example used to find out which global variables
-- need to be added to an expression prior to evaluating it.
scanCellRefs :: LC v -> [Pos]
scanCellRefs (CVar p)    = [p]
scanCellRefs (Lam _ e)   = scanCellRefs e
scanCellRefs (App e1 e2) = scanCellRefs e1 ++ scanCellRefs e2
scanCellRefs _ = []
