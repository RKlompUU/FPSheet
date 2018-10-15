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

import Debug.Trace


type V = VarT
type E = ExprT V
type C = CellT E
type S = Sheet C

instance Spreadsheet S C E V (StateT S IO) (StateT (M.Map V E) IO) where
  updateEvals = do
    s <- get
    undefined -- mapM_ updateEval (M.toList s)
  getCell p = do
    s <- get
    return (M.lookup p s)
  setCell p c = do
    s <- get
    put (M.insert p c s)

{-

-- | A naive way of fully (re)evaluating the cell expressions. If an
-- evaluation differs from the prior evaluation, the entire sheet will again
-- be evaluated. This is repeated until none of the evaluations differ from
-- the prior evaluation.
updateEval :: (Cell C E V mInner,
               Spreadsheet s C E V m mInner) =>
              (Pos, C) -> m ()
updateEval (p, c) = do
  let freshParse = c -- parseCell c
  s <- trace ("Updating: " ++ show p) get
  mC <- getCell p
  let refs = case (getEval freshParse) of
              Just e  -> trace ("Scanning refs in: " ++ show e) (refsInExpr e)
              Nothing -> trace "No evaluation" []
  refCs <- catMaybes
        <$> map preCat
        <$> zip refs
        <$> trace ("Found refs; " ++ show refs) mapM getCell refs
  let refEs = map (\(p,c :: CellT (ExprT VarT)) -> (p,getEval c)) refCs
  let mC' = mC >>= \c -> return $ c -- parseCell c
      globVars = mapMaybe (\(p,mE) -> mE >>= \e -> trace ("cRefs: " ++ show p) return (posToRef p, e)) refEs
      mC'' = trace ("test: " ++ show globVars) mC' >>= \c' -> return $ runReader (evalCell c') (M.fromList globVars)
      oldEval = mC >>= \c -> getEval c
      newEval = mC'' >>= \c'' -> getEval c''
  if (oldEval == newEval)
    then return ()
    else let c'' = case newEval of
                     Just e -> trace ("Updating e to: " ++ show e) c {cUFlag = True, cExpr = Just $ e}
                     Nothing -> c {cUFlag = True, cExpr = Nothing}
         in setCell p c'' >> updateEvals
  where preCat (p,Just j) = Just (p, j)
        preCat (p,Nothing) = Nothing
-}

instance Cell C E V (StateT (Env V E) IO) where
  evalCell c = do
    res <- I.runInterpreter $ I.setImports ["Prelude"] >> I.eval (cStr c)
    case res of
      Left _ -> return $ c {cExpr = Nothing}
      Right value -> return $ c {cExpr = Just (ExprT value)}
  getEval = cExpr
  getText = cStr

instance Var V where
  posToRef (c,r) =
    "(" ++ show c ++ "," ++ show r ++ ")"
instance Expr E V (StateT (Env V E) IO) where
  evalExpr e = do
    --env <- ask
    return e -- (fromIdInt $ nf $ toIdInt $ addCellRefs (M.assocs env) e)
  refsInExpr e =
    [] -- TODO




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

emptyCell :: C
emptyCell = CellT "" Nothing False
