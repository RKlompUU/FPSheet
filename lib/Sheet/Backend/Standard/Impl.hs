{-|
Module      : SpreedSheet.Sheet
Description : An experimental application of the spreadsheet API
Stability   : experimental
-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
             ScopedTypeVariables #-}
module Sheet.Backend.Standard.Impl (
  module Sheet.Backend.Standard.Impl,
  module Sheet.Backend.Standard.Types,
) where


import Data.Maybe

import Control.Monad
import Control.Monad.Reader

import qualified Data.Map as M
import qualified Data.Set as S

import Sheet.Backend.SheetAbstr
import Sheet.Backend.Standard.Types

import qualified Language.Haskell.Interpreter as I
import qualified Language.Haskell.Exts.Parser as P

import Debug.Trace


type VAR = VarT
type VAL = String
type E = ExprT V
type C = CellT E
type S = Sheet C

instance Spreadsheet S (StateT S IO) C E VAR VAL Pos where
  getCell p = do
    s <- get
    return (maybe (newCell p) id (M.lookup p s))
  setCell p c = do
    s <- get
    put (M.insert p c s)
  getSetCells = do
    s <- get
    return $ M.elems s

instance Cell S (StateT S IO) C E VAR VAL Pos where
  evalCell c = do
    if (cUFlag c)
      then return ()
      else do
        let e = getText c
        val <- evalExpr e
        newRes <- case val of
                    Left err -> Nothing
                    Right res -> Just res
        setCell (getCellPos c) (c { cRes = newRes, cUFlag = True })
        mapM (\p -> getCell p >>= evalCell) (refsInExpr e)
  getEval = cRes
  getText = cStr
  getCellPos = cPos
  newCell p = CellT "" Nothing False p

instance Var VAR VAL Pos where
  posToRef (c,r) =
    "(" ++ show c ++ "," ++ show r ++ ")"
instance Expr S (StateT S IO) E VAR VAL Pos where
  evalExpr eStr = do
    res <- I.runInterpreter $ I.setImports ["Prelude"] >> I.eval (cStr c)
    case res of
      Left _ -> return $ Left "Failed to evaluate expression: " ++ eStr
      Right res' -> return $ Right res'
  refsInExpr eStr =
    case parseExp eStr of
      ParseFailed _ _ -> [] -- No refs
      ParseOk p ->
        let constrs = filter grabDataConstructors p
        where grabDataConstructors d = undefined -- TODO   http://hackage.haskell.org/package/haskell-src-exts-1.20.3/docs/Language-Haskell-Exts-Syntax.html#t:Exp

-- | 'isInBox' if the position is inside the box 'Nothing' is returned.
-- Otherwise, it returns the offset of the position towards the box.
isInBox :: Pos -> (Pos,Pos) -> Maybe Pos
isInBox (r,c) ((rL, cL), (rH, cH)) =
  let rOffset = if r < rL
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
grabUpdatedCells :: S -> S
grabUpdatedCells = M.filter cUFlag

-- | 'resetUpdateFields' removes the update flags of all cells.
resetUpdateFields :: S -> S
resetUpdateFields = M.map (\c -> c {cUFlag = False})

-- | Subtraction on 'Pos' variables.
posSubtr :: Pos -> Pos -> Pos
posSubtr (r1,c1) (r2,c2) = (r1-r2,c1-c2)

-- | Addition on 'Pos' variables.
posAdd :: Pos -> Pos -> Pos
posAdd (r1,c1) (r2,c2) = (r1+r2,c1+c2)

-- | 'sliceList' grabs a part of list 'xs' that ranges from index 'from' to
-- index 'to'.
sliceList :: Int -> Int -> [a] -> [a]
sliceList from to xs =
  take (to - from + 1) (drop from xs)

-- | 'subLists' slices a list 'xs', where each slice has a length of at most
-- 'i'.
subLists :: Int -> [a] -> [[a]]
subLists i xs =
  let is = [0,i..(length xs - 1)]
  in map (\i' -> sliceList i' (i'+i-1) xs) is

initSheet :: S
initSheet = M.empty

-- | Helper function to conveniently obtain a 'CellT e' from the 'Sheet e'.
getSheetCell :: Pos -> S -> C
getSheetCell pos cs =
  M.findWithDefault emptyCell pos cs
