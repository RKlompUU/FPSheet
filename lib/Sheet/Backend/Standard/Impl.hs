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

import Sheet.Backend.Standard.Saves
import Sheet.Backend.Standard.Parsers

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
import qualified Language.Haskell.Exts.ExactPrint as P
import qualified Language.Haskell.Exts.SrcLoc as P
import qualified Language.Haskell.Exts.Pretty as P

import Language.Haskell.Exts.Util

import Control.Monad.Catch as MC

instance Spreadsheet S StateTy C E VAR VAL (Dep Pos) Pos where
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
  importFile f simpleImport = do
    c <- liftIO $ importCells f simpleImport
    s <- get
    put $ s {s_cells = c}
    reval
    return ()
  save f = do
    s <- get
    liftIO $ saveSheet s f
  load f = do
    res <- liftIO $ loadSheet f
    case res of
      Just save -> do
        s <- get
        put $ s {s_cells = save_cells save}
        reval
      Nothing -> return ()
  reval = do
    s <- get
    put $ s { s_deps = M.empty }
    cells <- getSetCells
    mapM_ revalCell cells
  interrupt = do
    s <- get
    liftIO $ throwTo (s_ghciThread s) InterpreterInterrupt

data Interrupt = InterpreterInterrupt
    deriving Show

instance Exception Interrupt

instance Cell S StateTy C E VAR VAL (Dep Pos) Pos where
  evalCell c = do
    let cPos = getCellPos c
    if (c_uFlag c)
      then return ()
      else do
        feedback <- s_visualFeedback <$> get
        liftIO $ feedback c CellUpdating
        -- Set uFlag to true, to prevent infinite recursion into cyclic cell dependencies
        setCell (c { c_uFlag = True, c_res = Nothing })

        jobChan <- s_jobsChan <$> get
        rangeableCells <- map (posToRef . getCellPos)
                      <$> filter (not . null . getText)
                      <$> getSetCells
        let (_, e) = preprocessExprStr (getText c) rangeableCells
        let j = BackendJob (posToRef cPos) e $
                  \resCode res -> do
                    c' <- getCell cPos
                    setCell (c' { c_res = res })
                    case resCode of
                      JobDefFailure -> liftIO $ feedback c' CellFailure
                      JobShowFailure -> liftIO $ feedback c' CellDefined
                      _ -> liftIO $ feedback c' CellSuccess
                    return ()

        liftIO $ writeChan jobChan j

        deps <- getCellDeps c
        mapM_ evalCell deps

        getCell cPos >>= \c' -> setCell (c' { c_uFlag = False })
        return ()
  revalCell c =
    let str = getText c
    in setText "" c >>= setText str >>= evalCell
  getEval = c_res
  getText = c_str
  setText str c = do
    rangeableCells <- map (posToRef . getCellPos)
                  <$> filter (not . null . getText)
                  <$> getSetCells
    let (oldRangeDeps, _) = preprocessExprStr (c_str c) rangeableCells
    let (newRangeDeps, _) = preprocessExprStr str rangeableCells
    let oldDeps = S.toList $ refsInExpr (c_str c)
        newDeps = S.toList $ refsInExpr str
        expired = map DepPos (oldDeps \\ newDeps)
               ++ (oldRangeDeps \\ newRangeDeps)
        appended = map DepPos (newDeps \\ oldDeps)
                ++ (newRangeDeps \\ oldRangeDeps)
    mapM_ (delCellDep c) expired
    mapM_ (addCellDep c) appended
    deps <- s_deps <$> get
    setCell (c {c_str = str})
  getCellPos = c_pos
  newCell p = CellT "" Nothing False p
  addCellDep c dep = do
    s <- get
    let deps = maybe [] id (M.lookup (c_pos c) (s_deps s))
    put $ s {s_deps = M.insert (c_pos c) (dep:deps) (s_deps s)}
  delCellDep c dep = do
    s <- get
    let deps = maybe [] id (M.lookup (c_pos c) (s_deps s))
    put $ s {s_deps = M.insert (c_pos c) (filter (/= dep) deps) (s_deps s)}
  getCellDeps c = do
    s <- get
    mapM getCell $
      nub $
      M.keys $
      M.filter (any (posInDep (c_pos c))) (s_deps s)

posInDep :: Pos -> Dep Pos -> Bool
posInDep p (DepPos depAt) = p == depAt
posInDep p (DepRange depFrom depTo) =
  col p >= col depFrom && col p <= col depTo &&
  row p >= row depFrom && row p <= row depTo
posInDep p (DepRangeDown depFrom) =
  col p == col depFrom &&
  row p >= row depFrom

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

initSheet :: (BackendJobResponse -> IO ()) -> (C -> CellStatus -> IO ()) -> IO S
initSheet asyncResFunc visualFeedbackFunc = do
  jobChan <- newChan
  resChan <- newChan
  ghciThreadID <- forkIO (ghciThread jobChan asyncResFunc)
  return $ Sheet M.empty M.empty jobChan visualFeedbackFunc ghciThreadID

data IState = IState {
  istate_imports :: [String],
  istate_loads   :: [String]
} deriving Show

ghciThread :: ChanJobs -> (BackendJobResponse -> IO ()) -> IO ()
ghciThread jobs respF = do
  crash <- I.runInterpreter $ do
    I.setImports ["Prelude"]
    liftIO $ ghciLog $ ";\n;\n"
    let initState = IState ["Prelude"] []
    flip loop initState $ \state -> do
      flip MC.catch (catchInterrupt state) $ do
        j <- liftIO $ readChan jobs

        liftIO $ ghciLog $
          "------------------------\nNew job:\n"

        (state', res') <- case parseCellDef (bJob_cDef j) of
          Just (LetDef str) -> letdef j str state
          Just (Import str) -> importModule j str state
          Just (Load   str) -> loadModule j str state
          Just (IODef  str) -> iodef j str state
        liftIO $ respF res'
        return state'
  ghciLog (show crash)
  return ()
  where catchDefErr :: IState -> BackendJob -> SomeException -> I.Interpreter (IState, BackendJobResponse)
        catchDefErr s j e = do
          liftIO $ ghciLog ("***********\n" ++ show e ++ "\n*************\n")
          I.runStmt $ "let " ++ bJob_cName j ++ " = undefined"
          return $ (s, BackendJobResponse (bJob_resBody j JobDefFailure Nothing))
        catchShowErr :: IState -> BackendJob -> SomeException -> I.Interpreter (IState, BackendJobResponse)
        catchShowErr s j e = do
          liftIO $ ghciLog ("\t" ++ show e ++ "\n")
          return $ (s, BackendJobResponse (bJob_resBody j JobShowFailure Nothing))
        catchInterrupt :: a -> Interrupt -> I.Interpreter a
        catchInterrupt x e = return x

        importModule :: BackendJob -> String -> IState -> I.Interpreter (IState, BackendJobResponse)
        importModule j m s = do
          let i' = nub $ m : istate_imports s
          let s' = s { istate_imports = i' }
          flip MC.catch (catchDefErr s j) $ do
            I.setImports i'
            return $ (s', BackendJobResponse (bJob_resBody j JobSuccess (Just m)))

        loadModule :: BackendJob -> String -> IState -> I.Interpreter (IState, BackendJobResponse)
        loadModule j m s = do
          let l' = nub $ m : istate_loads s
          let s' = s { istate_loads = l' }
          flip MC.catch (catchDefErr s j) $ do
            I.loadModules l'
            I.setTopLevelModules $ map (\m_ -> take (fromJust . findIndex (=='.') $ m_) m_) l'
            return $ (s', BackendJobResponse (bJob_resBody j JobSuccess (Just m)))

        iodef :: BackendJob -> String -> IState -> I.Interpreter (IState, BackendJobResponse)
        iodef j cdef s = do
          let ioDef = bJob_cName j ++ " <- " ++ cdef
          liftIO $ ghciLog $
            "\t" ++ ioDef ++ "\n"
          flip MC.catch (catchDefErr s j) $ do
            I.runStmt ioDef
            liftIO $ ghciLog $
              "\tioDef executed\n"

            flip MC.catch (catchShowErr s j) $ do
              res <- I.eval (bJob_cName j)
              liftIO $ ghciLog $
                "\tres: " ++ show res ++ "\n"
              return $ (s, BackendJobResponse (bJob_resBody j JobSuccess (Just res)))

        letdef :: BackendJob -> String -> IState -> I.Interpreter (IState, BackendJobResponse)
        letdef j cdef s = do
          let letDef = "let " ++ bJob_cName j ++ " = " ++ cdef
          liftIO $ ghciLog $
            "\t" ++ letDef ++ "\n"
          flip MC.catch (catchDefErr s j) $ do
            I.runStmt letDef

            liftIO $ ghciLog $
              "\tletDef executed\n"

            flip MC.catch (catchShowErr s j) $ do
              res <- I.eval (bJob_cName j)
              liftIO $ ghciLog $
                "\tres: " ++ show res ++ "\n"
              return $ (s, BackendJobResponse (bJob_resBody j JobSuccess (Just res)))

ghciLog :: String -> IO ()
ghciLog str = do
  appendFile "/tmp/fpsheet_ghci.log" str

appLog :: String -> IO ()
appLog str = do
  appendFile "/tmp/fpsheet_app.log" str

loop :: Monad m => (a -> m a) -> a -> m a
loop action x = action x >>= loop action


preprocessExprStr :: String -> [String] -> ([Dep Pos], String)
preprocessExprStr eStr rangeableCells =
  case P.parseExp eStr of
    P.ParseFailed _ _ -> ([], eStr)
    P.ParseOk p ->
      let (dependencyRanges, p') = preprocessExpr p [] rangeableCells
      in (dependencyRanges, P.prettyPrintStyleMode (P.style {P.mode = P.LeftMode}) P.defaultMode p')

preprocessExpr :: P.Exp P.SrcSpanInfo -> [String] -> [String] -> ([Dep Pos], P.Exp P.SrcSpanInfo)
preprocessExpr e@(P.EnumFromTo _ enumFrom enumTo) unfree rangeableCells =
  let posFrom = posRef enumFrom
      posTo   = posRef enumTo
      deps =
        if isJust posFrom && isJust posTo
          then [DepRange (fromJust posFrom) (fromJust posTo)]
          else []
      rangeCells =
        if isJust posFrom && isJust posTo
          then let r = filter (\c -> any (==c) rangeableCells)
                     $ map posToRef
                     $ rangePos (fromJust posFrom) (fromJust posTo)
               in if isInfixOf r unfree
                    then Nothing
                    else Just r
          else Nothing
  in if isJust rangeCells
      then (,) deps
         $ P.List P.noSrcSpan
         $ map (P.Var P.noSrcSpan . P.UnQual P.noSrcSpan . P.Ident P.noSrcSpan)
         $ fromJust rangeCells
      else (deps, e)
preprocessExpr e@(P.EnumFrom l enumFrom) unfree rangeableCells =
  let posFrom = posRef enumFrom
      deps =
        if isJust posFrom
          then [DepRangeDown (fromJust posFrom)]
          else []
      rangeCells =
        if isJust posFrom
          then let maxPos = maximum
                          $ (:) (fromJust posFrom)
                          $ filter (\(c,_) -> c == (col . fromJust) posFrom)
                          $ map (fromJust . parsePos) rangeableCells
                   r = filter (\c -> any (==c) rangeableCells)
                     $ map posToRef
                     $ rangePos (fromJust posFrom) maxPos
               in if isInfixOf r unfree
                    then Nothing
                    else Just r
          else Nothing
  in if isJust rangeCells
      then (,) deps
         $ P.List P.noSrcSpan
         $ map (P.Var P.noSrcSpan . P.UnQual P.noSrcSpan . P.Ident P.noSrcSpan)
         $ fromJust rangeCells
      else (deps, e)
preprocessExpr (P.App l e1 e2) unfree rangeableCells =
  let (rs1, e1') = preprocessExpr e1 unfree rangeableCells
      (rs2, e2') = preprocessExpr e2 unfree rangeableCells
  in (rs1++rs2, P.App l e1' e2')
preprocessExpr (P.Let l binds e) unfree rangeableCells =
  let v = map unName
        $ S.toList
        $ bound
        $ allVars binds
      (rs, e') = preprocessExpr e (unfree ++ v) rangeableCells
  in (rs, P.Let l binds e')
preprocessExpr (P.InfixApp l e1 op e2) unfree rangeableCells =
  let (rs1, e1') = preprocessExpr e1 unfree rangeableCells
      (rs2, e2') = preprocessExpr e2 unfree rangeableCells
  in (rs1++rs2, P.InfixApp l e1' op e2')
preprocessExpr (P.Lambda l patterns e) unfree rangeableCells =
  let v = map unName
        $ S.toList
        $ bound
        $ allVars patterns
      (rs, e') = preprocessExpr e (unfree ++ v) rangeableCells
  in (rs, P.Lambda l patterns e')
preprocessExpr e _ _ = ([], e)

unName :: P.Name l -> String
unName (P.Ident _ n) = n
unName (P.Symbol _ n) = n

posRef :: P.Exp P.SrcSpanInfo -> Maybe Pos
posRef (P.Var _ (P.UnQual _ (P.Ident _ str))) = parsePos str
posRef _ = Nothing

rangePos :: Pos -> Pos -> [Pos]
rangePos (c1,r1) (c2,r2) =
  [(c, r) | c <- [c1..c2], r <- [r1..r2]]

col :: Pos -> Int
col = fst

row :: Pos -> Int
row = snd
