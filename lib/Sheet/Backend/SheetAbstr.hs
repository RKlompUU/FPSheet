{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes,
             ConstraintKinds, FlexibleContexts #-}
module Sheet.Backend.SheetAbstr
  ( module Sheet.Backend.SheetAbstr
  , module Control.Monad
  , module Control.Monad.State
  , module Data.Functor.Identity
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Data.Functor.Identity

import Data.Map.Lazy

import qualified Data.Set as S


-- Annotated text (with for example explicit information about cells that are referred to)
--class AnnText t where

-- | The Spreadsheet API interface supplies toplevel functions.
class (MonadState s m, Var var pos, Expr s m e var val pos, Cell s m c e var val dep pos) => Spreadsheet s m c e var val dep pos | s -> c, s -> var, s -> e, s -> m where
  -- | 'getSetCells' returns the list of thus far set cells
  getSetCells :: m [c]
  -- | 'getCell' retrieves a cell from the spreadsheet.
  getDepGraph :: [c] -> m [(pos,[pos])]
  getCell :: pos -> m c
  -- | 'setCell' sets a 'Cell' c in the spreadsheet at the 'Pos' that must be retrievable from within c.
  -- If a 'Cell' at the given 'Pos' was already present, it is overwritten.
  setCell :: c -> m c
  evalCells :: [c] -> m ()
  -- | 'reval' reevalutes all Cells
  reval :: m ()
  -- | 'importFile' loads an external format from disk (currently only supports .xlsx)
  importFile :: String -> Bool -> m ()
  -- | 'save' saves the sheet state to disk (in FPSheet's own format)
  save :: String -> m ()
  -- | 'load' loads the sheet state from disk (in FPSheet's own format)
  load :: String -> m ()
  interrupt :: m ()


-- | The 'Cell' API interface supplies cell manipulation functions.
class (MonadState s m, Var var pos, Expr s m e var val pos) => Cell s m c e var val dep pos | c -> e, c -> dep, c -> var, var -> m, e -> m where
  -- | 'evalCell' tries to evaluate the cell's content, in the context of the current spreadsheet's state.
  -- This is run in the state monad. 'evalCell' must change the evaluated cell in the spreadsheet state. Possibly,
  -- depending on the implementation choices made, it additionally re-evaluates those cells that are depending on a
  -- currently evaluated cell.
  -- | 'getEval' returns the evaluation that has been determined during
  evalCell :: c -> m ()
  -- a prior call to 'evalCell' if it resulted in an evaluation. Otherwise
  -- 'getEval' returns 'Nothing'.
  getEval :: c -> Maybe String
  -- | 'getText' returns the text contents of a 'Cell'.
  getText :: c -> String
  -- | 'setText' sets textual definition of the cell (note: this does not trigger evaluation)
  setText :: String -> c -> m c
  -- | 'getCellPos' returns the position on the sheet of the cell
  getCellPos :: c -> pos
  -- | 'newCell' returns a new cell (probably an empty cell, but this is a choice left for the implementation).
  newCell :: pos -> c
  -- dependencies
  addCellDep :: c -> dep -> m ()
  delCellDep :: c -> dep -> m ()
  getCellDeps :: c -> m [c]


-- | The 'Expr' API interface supplies expression manipulation functions.
class (MonadState s m, Var var pos) => Expr s m e var val pos | e -> val, e -> pos, e -> var, var -> m, e -> m where
  refsInExpr :: e -> S.Set pos


-- | The 'Var' API interface is currently purely used to allow for different
-- kind of variable encodings within languages. Perhaps this part of the
-- API should be extended with functions once some kind of annotated text
-- mechanism has been added.
class Var var pos where
  posToRef :: pos -> var
