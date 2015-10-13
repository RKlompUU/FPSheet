{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Src.API.SheetAbstr where


type Pos = (Int,Int)

class (Var v, Expr e v, Cell c e v) => Spreadsheet s c e v | s -> c, s -> v where
  updateEvals :: s -> s
  getCell :: s -> Pos -> Maybe c
  setCell :: s -> Pos -> c -> s
  --derefExpr :: s -> e -> e
  --derefRef :: s -> r -> c
  --refCell :: s -> Pos -> r


class (Var v, Expr e v) => Cell c e v | c -> e, c -> v where
  evalCell :: c -> c
  setGlobalVars :: c -> [(v,e)] -> c
  parseCell :: c -> c
  getEval :: c -> Maybe e
  getText :: c -> String


class Ref r where
  deref :: r -> Pos

class Var v => Expr e v | e -> v where
  addGlobalVar :: e -> v -> e -> e
  cleanGlobalVars :: e -> e
  evalExpr :: v -> e -> e

class Var v where


