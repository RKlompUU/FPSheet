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
import Control.Monad.Reader

import Data.Functor.Identity

import Data.Map

-- | Each Cell is positioned on a 2-dimensional grid. Its position is
-- defined by the ith row and jth column, where i and j are \"(i, j) :: 'Pos'\"
type Pos = (Int,Int)

type Env v e = Map v e


-- Annotated text (with for example explicit information about cells that are referred to)
--class AnnText t where


-- | The Spreadsheet API interface supplies toplevel functions.
class (Var v, Expr e v, Cell c e v) => Spreadsheet s c e v | s -> c, s -> v, s -> e where
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
  setCell :: Pos -> c -> State s ()

-- | The 'Cell' API interface supplies cell manipulation functions.
class (Var v, Expr e v) => Cell c e v | c -> e, c -> v where
  -- | 'evalCell' tries to evaluate the expression that it contains.
  -- Prior to calling this, the cell's textual contents need to have been
  -- parsed using the 'parseCell' function.
  --
  ---- | 'setGlobalVars' sets the global variables 'v' along with their
  -- definitions 'e'. Subsequent calls to 'evalCell' will be able to use
  -- this.
  evalCell :: c -> Reader (Env v e) c
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
  -- | 'evalExpr' evaluates the expression. Currently this part of the API
  -- expects that succesfully evaluating an expression will result in
  -- another expression of the same language. It might be desirable to
  -- change this function\'s type signature should this expection be(come)
  -- invalid.
  evalExpr :: e -> Reader (Env v e) e

-- | The 'Var' API interface is currently purely used to allow for different
-- kind of variable encodings within languages. Perhaps this part of the
-- API should be extended with functions once some kind of annotated text
-- mechanism has been added.
class Var v where


