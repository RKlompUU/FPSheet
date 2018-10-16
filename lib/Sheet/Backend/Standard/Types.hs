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


-- | Each Cell is positioned on a 2-dimensional grid. Its position is
-- defined by the ith col and jth row, where i and j are \"(i, j) :: 'Pos'\"
type Pos = (Int,Int)

data CellT e =
  CellT { cStr   :: String -- |User defined cell's text
        , cRes   :: Maybe e -- |The result of the last evaluation of cStr
        , cUFlag :: Bool -- |Cell has changed, used to check if an input field needs to be refreshed by the frontend
        , cPos   :: Pos
  } deriving (Show)

type Sheet c = Map Pos c

type ExprT v = String

type VarT = String
