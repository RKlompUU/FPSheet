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
  CellT { str  :: String
        , lExpr :: Maybe (e)
        , uFlag :: Bool -- Cell has changed, used to check if an input field needs to be refreshed by the frontend
  }

type StringCell = CellT String

type Sheet e = Map Pos (CellT e)
