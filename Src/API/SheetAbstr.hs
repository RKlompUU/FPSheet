{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes,
             ConstraintKinds #-}
module API.SheetAbstr
  ( module API.SheetAbstr
  , module Control.Monad
  , module Control.Monad.State
  , module Data.Functor.Identity
  ) where

import Control.Monad
import Control.Monad.State

import Data.Functor.Identity

-- | Each Cell is positioned on a 2-dimensional grid. Its position is
-- defined by the ith row and jth column, where i and j are \"(i, j) :: 'Pos'\"
type Pos = (Int,Int)

-- Annotated text (with for example explicit information about cells that are referred to)
--class AnnText t where


-- | The Spreadsheet API interface supplies toplevel functions.
class (MonadState s m, Var v, Expr e v, Cell c e v) => Spreadsheet s c e v m | s -> c, s -> v, s -> m where
  -- | 'updateEvals' performs a full update (it evaluates each cell).
  -- The current implementation that has been used to experiment with this
  -- API performs naive updates. It might be a good idea to change this
  -- part of the API to allow for other update strategies being used
  -- (updateEvals currently doesn't accept a list of changes that it has to
  -- act upon). Although other update strategies could be applied with the
  -- current API as well by saving additional information inside the
  -- Spreadsheet datatype 's'.
  updateEvals :: State s ()
  -- | 'getCell' retrieves a cell from the spreadsheet.
  getCell :: Pos -> State s (Maybe c)
  -- | 'setCell' sets a 'Cell' c at 'Pos' in the spreadsheet. If a 'Cell' at the given 'Pos' was already present, it is overwritten.
  setCell :: s -> Pos -> c -> s

-- | The 'Cell' API interface supplies cell manipulation functions.
class (Var v, Expr e v) => Cell c e v | c -> e, c -> v where
  -- | 'evalCell' tries to evaluate the expression that it contains.
  -- Prior to calling this, the cell's textual contents need to have been
  -- parsed using the 'parseCell' function.
  evalCell :: c -> c
  -- | 'setGlobalVars' sets the global variables 'v' along with their
  -- definitions 'e'. Subsequent calls to 'evalCell' will be able to use
  -- this.
  setGlobalVars :: c -> [(v,e)] -> c
  -- | 'parseCell' Tries to parse the textual contents of 'Cell' c.
  parseCell :: c -> c
  -- | 'getEval' returns the evaluation that has been determined during
  -- a prior call to 'evalCell' if it resulted in an evaluation. Otherwise
  -- 'getEval' returns 'Nothing'.
  getEval :: c -> Maybe e
  -- | 'getText' returns the textual contents of a 'Cell'.
  getText :: c -> String


-- | The 'Expr' API interface supplies expression manipulation functions.
class Var v => Expr e v | e -> v where
  -- | 'addGlobalVar' adds a global variable along with its definition to
  -- the expression. All global variables that are required for a succesful
  -- evaluation of the expression should be given through this function
  -- prior to calling the 'evalExpr' function.
  addGlobalVar :: e -> v -> State [(e,v)] ()
  -- | 'cleanGlobalVars' removes any prior added global variables along
  -- with their definitions from the expression.
  cleanGlobalVars :: State [(e,v)] ()
  -- | 'evalExpr' evaluates the expression. Currently this part of the API
  -- expects that succesfully evaluating an expression will result in
  -- another expression of the same language. It might be desirable to
  -- change this function\'s type signature should this expection be(come)
  -- invalid.
  evalExpr :: e -> State [(e,v)] e

-- | The 'Var' API interface is currently purely used to allow for different
-- kind of variable encodings within languages. Perhaps this part of the
-- API should be extended with functions once some kind of annotated text
-- mechanism has been added.
class Var v where


