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
import Data.List

import Control.Monad
import Control.Monad.Reader

import qualified Data.Map as M
import qualified Data.Set as S

import Sheet.Backend.SheetAbstr
import Sheet.Backend.Standard.Types

import Control.Concurrent.Chan
import Control.Concurrent

import qualified Language.Haskell.Interpreter as I
import qualified Language.Haskell.Exts.Parser as P
import qualified Language.Haskell.Exts.Syntax as P
import Language.Haskell.Exts.Util

import Control.Monad.Catch as MC

instance Spreadsheet S StateTy C E VAR VAL Pos where
  getCell p = do
    cells <- s_cells <$> get
    return (maybe (newCell p) id (M.lookup p cells))
  setCell c = do
    s <- get
    put $ s {s_cells = M.insert (c_pos c) c (s_cells s)}
    return c
  getSetCells = do
    cells <- s_cells <$> get
    return $ M.elems cells

instance Cell S StateTy C E VAR VAL Pos where
  evalCell c = do
    let cPos = getCellPos c
    if (c_uFlag c)
      then return ()
      else do
        -- Set uFlag to true, to prevent infinite recursion into cyclic cell dependencies
        setCell (c { c_uFlag = True })

        jobChan <- s_jobsChan <$> get
        let e = getText c
        let j = BackendJob (posToRef cPos) e $
                  \res -> do
                    getCell cPos >>= \c' -> setCell (c' { c_res = res })
                    return ()

        liftIO $ writeChan jobChan j

        deps <- getCellDeps (getCellPos c)
        mapM_ (\p -> getCell p >>= evalCell) deps

        getCell cPos >>= \c' -> setCell (c' { c_uFlag = False })
        return ()
  getEval = c_res
  getText = c_str
  setText str c = do
    let oldDeps = S.toList $ refsInExpr (c_str c)
        newDeps = S.toList $ refsInExpr str
        expired = oldDeps \\ newDeps
        appended = newDeps \\ oldDeps
    mapM_ (delCellDep (c_pos c)) expired
    mapM_ (addCellDep (c_pos c)) appended
    setCell (c {c_str = str})
  getCellPos = c_pos
  newCell p = CellT "" Nothing False p

instance Var VAR Pos where
  posToRef (c,r) =
    toCol c ++ show r
instance Expr S StateTy E VAR VAL Pos where
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


addCellDep :: Pos -> Pos -> StateTy ()
addCellDep p1 p2 = do
  deps <- getCellDeps p2
  s <- get
  put $ s {s_deps = M.insert p2 (nub $ p1:deps) (s_deps s)}

delCellDep :: Pos -> Pos -> StateTy ()
delCellDep p1 p2 = do
  deps <- getCellDeps p2
  s <- get
  put $ s {s_deps = M.insert p2 (filter (/= p1) deps) (s_deps s)}


getCellDeps :: Pos -> StateTy [Pos]
getCellDeps p = do
  s <- get
  return $ maybe [] id (M.lookup p (s_deps s))


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
-- toInt "A" = 1
-- toInt "XFD" = 16384
toInt :: String -> Int
toInt = foldl fn 0
  where
    fn = \a c -> 26*a + ((ord c)-(ord 'a' - 1))

toCol :: Int -> String
toCol c = ([0..] >>= flip replicateM ['a'..'z']) !! c

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

-- | 'resetUpdateFields' removes the update flags of all cells.
resetUpdateFields :: S -> S
resetUpdateFields s = s {s_cells = M.map (\c -> c {c_uFlag = False}) (s_cells s)}

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

initSheet :: (BackendJobResponse -> IO ()) -> IO S
initSheet asyncResFunc = do
  jobChan <- newChan
  resChan <- newChan
  forkIO (ghciThread jobChan asyncResFunc)
  return $ Sheet M.empty M.empty jobChan

ghciThread :: ChanJobs -> (BackendJobResponse -> IO ()) -> IO ()
ghciThread jobs respF = do
  crash <- I.runInterpreter $ do
    I.setImports ["Prelude"]
    loop $ do
      j <- liftIO $ readChan jobs

      res' <- do
          let letDef = "let " ++ bJob_cName j ++ " = " ++ bJob_cDef j
          liftIO $ ghciLog $
            "\t" ++ letDef ++ "\n"
          flip MC.catch (catchDefErr j) $ do
            I.runStmt letDef

            liftIO $ ghciLog $
              "\tletDef executed\n"

            flip MC.catch (catchShowErr j) $ do
              res <- I.eval (bJob_cName j)
              liftIO $ ghciLog $
                "\tres: " ++ show res ++ "\n"
              return $ BackendJobResponse (bJob_resBody j (Just res))
      liftIO $ respF res'
  ghciLog (show crash)
  return ()
  where catchDefErr :: BackendJob -> SomeException -> I.Interpreter BackendJobResponse
        catchDefErr j e = do
          liftIO $ ghciLog ("\t" ++ show e ++ "\n")
          I.runStmt $ "let " ++ bJob_cName j ++ " = undefined"
          return $ BackendJobResponse (bJob_resBody j Nothing)
        catchShowErr :: BackendJob -> SomeException -> I.Interpreter BackendJobResponse
        catchShowErr j e = do
          liftIO $ ghciLog ("\t" ++ show e ++ "\n")
          return $ BackendJobResponse (bJob_resBody j Nothing)

ghciLog :: String -> IO ()
ghciLog str = do
  appendFile "/tmp/fpsheet_ghci.log" str

loop :: Monad m => m () -> m ()
loop action = action >> loop action
