module Sheet.Frontend.Types where

import Sheet.Backend.Standard

-- | 'UISheet' defines the spreadsheet type. The functions in this UI
-- submodule pass a value of this datatype along in a statewise matter.
data UISheet = UISheet { sheetCells  :: Sheet (ExprT String)
                       , sheetCursor :: Pos
                       , sheetOffset :: Pos }
