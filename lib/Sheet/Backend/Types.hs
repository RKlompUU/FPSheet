{-|
Module      : Sheet.Backend.Types
Description : A Sheet datatype, that contains a grid of cells
Stability   : experimental
-}
module Sheet.Backend.Types where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M


import Sheet.Backend.SheetAbstr

data CellT e =
  CellT { cStr   :: String -- |User defined cell's text
        , cExpr  :: Maybe (e) -- |The result from parsing (and executing?) str
        , cUFlag :: Bool -- |Cell has changed, used to check if an input field needs to be refreshed by the frontend
  }

type Sheet e = Map Pos (CellT e)

data ExprT v =
  ExprT {
    parsedExpr :: String,
    evaluatedExpr :: String
  }

type VarT = String
