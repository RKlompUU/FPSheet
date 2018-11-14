module Sheet.Frontend.Types where

import Sheet.Backend.Standard

import Brick.Widgets.Edit

-- | 'UISheet' defines the spreadsheet type. The functions in this UI
-- submodule pass a value of this datatype along in a statewise matter.
data UISheet =
  UISheet {
    sheetCells  :: S,
    sheetCursor :: Pos,
    sheetOffset :: Pos,

    uiCols :: Int,
    uiRows :: Int,
    cWidth :: Int,

    uiMode :: UIMode
  }

data UIMode =
    ModeNormal
  | ModeEdit {
      cellEditor :: Editor String String,
      cellEditorWidth :: Int
    }

initUISheet =
  UISheet {
    sheetCells = initSheet,
    sheetCursor = (1,1),
    sheetOffset = (1,1),
    uiCols = 10,
    uiRows = 10,
    cWidth = 5,
    uiMode = ModeNormal
  }
