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


import qualified ParseLib.Simple as SimpleP

import Data.Maybe
import Data.Char

import Control.Monad
import Control.Monad.Reader

import qualified Data.Map as M
import qualified Data.Set as S

import Sheet.Backend.SheetAbstr
import Sheet.Backend.Standard.Types

import qualified Language.Haskell.Interpreter as I
import qualified Language.Haskell.Exts.Parser as P
import qualified Language.Haskell.Exts.Syntax as P
import Language.Haskell.Exts.Util

import Debug.Trace


type VAR = VarT
type VAL = String
type E = ExprT VAR
type C = CellT E
type S = Sheet C

instance Spreadsheet S (StateT S IO) C E VAR VAL Pos where
  getCell p = do
    s <- get
    return (maybe (newCell p) id (M.lookup p s))
  setCell c = do
    s <- get
    put (M.insert (cPos c) c s)
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
        let newRes = case val of
                      Left err -> Nothing
                      Right res -> Just res
        setCell (c { cRes = newRes, cUFlag = True })
        mapM_ (\p -> getCell p >>= evalCell) (refsInExpr e)
  getEval = cRes
  getText = cStr
  getCellPos = cPos
  newCell p = CellT "" Nothing False p

instance Var VAR Pos where
  posToRef (c,r) =
    "(" ++ show c ++ "," ++ show r ++ ")"
instance Expr S (StateT S IO) E VAR VAL Pos where
  evalExpr eStr = do
    res <- I.runInterpreter $ I.setImports ["Prelude"] >> I.eval eStr
    case res of
      Left _ -> return $ Left ("Failed to evaluate expression: " ++ eStr)
      Right res' -> return $ Right res'
  refsInExpr eStr =
    case P.parseExp eStr of
      P.ParseFailed _ _ -> S.empty
      P.ParseOk p ->
        let fv = freeVars p
        in S.map fromJust
          $ S.filter isJust
          $ S.map fv2Pos fv
        where fv2Pos (P.Ident _ str) = parsePos str
              fv2Pos (P.Symbol _ _) = Nothing

type SParser a = SimpleP.Parser Char a

parsePos :: String -> Maybe Pos
parsePos str =
  let res = SimpleP.parse posParser str
  in if null res
      then Nothing
      else Just $ fst $ head $ res

posParser :: SParser Pos
posParser =
  (,) SimpleP.<$> pCol SimpleP.<*> pRow

-- Might write this better (if it can be done better) later.
-- For now a copy paste from:
--  https://stackoverflow.com/questions/40950853/excel-column-to-int-and-vice-versa-improvements-sought
--
-- given a spreadsheet column as a string
-- returns integer giving the position of the column
-- ex:
-- toInt "A" = 0
-- toInt "XFD" = 16383
toInt :: String -> Int
toInt = foldl fn 0
  where
    fn = \a c -> 26*a + ((ord c)-(ord 'a'))

pCol :: SParser Int
pCol =
  toInt SimpleP.<$> SimpleP.some (SimpleP.satisfy (\c -> any (== c) ['a'..'z']))
pRow :: SParser Int
pRow =
  SimpleP.natural

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
