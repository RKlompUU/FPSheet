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
      P.ParseFailed _ _ -> [] -- No refs
      P.ParseOk p ->
        let constrs = grabDataConstructors p
        in [] -- error $ show p

cnstrsInStmnt :: Stmnt l -> [(String,Pos)]
cnstrsInStmnt (Generator _ _ e) = cnstrs e
cnstrsInStmnt (Qualifier _ e) = cnstrs e
cnstrsInStmnt _ = []

cnstrs :: Exp l -> [(String, Pos)]
cnstrs c@(Con l (QName l2)) = [undefined]
cnstrs (InfixApp _ e1 _ e2) = cnstrs e1 ++ cnstrs e2
cnstrs (App _ e1 e2) = cnstrs e1 ++ cnstrs e2
cnstrs (NegApp _ e) = cnstrs e
cnstrs (Lambda _ _ e) = cnstrs e
cnstrs (Let _ _ e) = cnstrs e
cnstrs (If _ ifE thenE elseE) = cnstrs ifE ++ cnstrs thenE ++ cnstrs elseE
cnstrs (MultiIf _ (GuardedRhs _ stmnts e) = concatMap cnstrsInStmnt stmnts ++ cnstrs e
cnstrs (Case _ e [Alt l]
Do l [Stmt l]
MDo l [Stmt l]
Tuple l Boxed [Exp l]
UnboxedSum l Int Int (Exp l)
TupleSection l Boxed [Maybe (Exp l)]
List l [Exp l]
ParArray l [Exp l]
Paren l (Exp l)
LeftSection l (Exp l) (QOp l)
RightSection l (QOp l) (Exp l)
RecConstr l (QName l) [FieldUpdate l]
RecUpdate l (Exp l) [FieldUpdate l]
EnumFrom l (Exp l)
EnumFromTo l (Exp l) (Exp l)
EnumFromThen l (Exp l) (Exp l)
EnumFromThenTo l (Exp l) (Exp l) (Exp l)
ParArrayFromTo l (Exp l) (Exp l)
ParArrayFromThenTo l (Exp l) (Exp l) (Exp l)
ListComp l (Exp l) [QualStmt l]
ParComp l (Exp l) [[QualStmt l]]
ParArrayComp l (Exp l) [[QualStmt l]]
ExpTypeSig l (Exp l) (Type l)
VarQuote l (QName l)
TypQuote l (QName l)
BracketExp l (Bracket l)
SpliceExp l (Splice l)
QuasiQuote l String String
TypeApp l (Type l)
XTag l (XName l) [XAttr l] (Maybe (Exp l)) [Exp l]
XETag l (XName l) [XAttr l] (Maybe (Exp l))
XPcdata l String
XExpTag l (Exp l)
XChildTag l [Exp l]
CorePragma l String (Exp l)
SCCPragma l String (Exp l)
GenPragma l String (Int, Int) (Int, Int) (Exp l)
Proc l (Pat l) (Exp l)
LeftArrApp l (Exp l) (Exp l)
RightArrApp l (Exp l) (Exp l)
LeftArrHighApp l (Exp l) (Exp l)
RightArrHighApp l (Exp l) (Exp l)
LCase l [Alt l]

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
