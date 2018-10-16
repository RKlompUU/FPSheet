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

-- | Each Cell is positioned on a 2-dimensional grid. Its position is
-- defined by the ith row and jth column, where i and j are \"(i, j) :: 'Pos'\"
type Pos = (Int,Int)

type Env v e = Map v e


-- Annotated text (with for example explicit information about cells that are referred to)
--class AnnText t where

-- | The Spreadsheet API interface supplies toplevel functions.
class (MonadState s m, Var v pos, Expr s m e v pos, Cell s m  c e v pos) => Spreadsheet s m c e v pos | s -> c, s -> v, s -> e, s -> m where
  -- | 'updateEvals' performs a full update (it evaluates each cell).
  updateEvals :: m ()
  -- | 'getCell' retrieves a cell from the spreadsheet.
  getCell :: pos -> m c
  -- | 'setCell' sets a 'Cell' c at 'Pos' in the spreadsheet.
  -- If a 'Cell' at the given 'Pos' was already present, it is overwritten.
  setCell :: pos -> c -> m ()

-- | The 'Cell' API interface supplies cell manipulation functions.
class (MonadState s m, Var v pos, Expr m e v pos) => Cell s m  c e v pos | c -> e, c -> v, v -> m, e -> m where
  -- | 'evalCell' tries to evaluate the cell's content, in the context of the current spreadsheet's state.
  -- This is run in the state monad. 'evalCell' should change the evaluated cell in the spreadsheet state,
  -- additionally (for a possible convenience) 'evalCell' returns the modified cell.
  evalCell :: c -> m c
  -- | 'getEval' returns the evaluation that has been determined during
  -- a prior call to 'evalCell' if it resulted in an evaluation. Otherwise
  -- 'getEval' returns 'Nothing'.
  getEval :: c -> Maybe e
  -- | 'getText' returns the textual contents of a 'Cell'.
  getText :: c -> String
  -- | 'newCell' returns an empty cell
  newCell :: c

-- | The 'Expr' API interface supplies expression manipulation functions.
class (MonadState s m, Var v pos) => Expr s m  e v pos | e -> v, v -> m, e -> m where
  -- | 'evalExpr' evaluates the expression. This function is run under a MonadReader
  -- environment, where values of required global variables should be
  -- available to read.
  evalExpr :: e -> m e
  refsInExpr :: e -> [Pos]

-- | The 'Var' API interface is currently purely used to allow for different
-- kind of variable encodings within languages. Perhaps this part of the
-- API should be extended with functions once some kind of annotated text
-- mechanism has been added.
class Var v pos where
  posToRef :: pos -> v
