{-|
Module      : Sheet.Backend.Types
Description : A Sheet datatype, that contains a grid of cells
Stability   : experimental
-}
module Sheet.Backend.Standard.Types where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M


import Sheet.Backend.SheetAbstr

data CellT e =
  CellT { cStr   :: String -- |User defined cell's text
        , cExpr  :: Maybe (e) -- |The result from parsing (and executing?) str
        , cUFlag :: Bool -- |Cell has changed, used to check if an input field needs to be refreshed by the frontend
  } deriving (Show)

type Sheet c = Map Pos c

data ExprT v =
  ExprT {
    evaluatedExpr :: String
  } deriving (Show, Eq)

type VarT = String
