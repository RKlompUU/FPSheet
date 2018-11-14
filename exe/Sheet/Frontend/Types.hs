module Sheet.Frontend.Types where

import Sheet.Backend.Standard

import Control.Concurrent.Chan
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

initUISheet :: IO UISheet
initUISheet = do
  sheet <- initSheet
  return $ UISheet {
    sheetCells = sheet,
    sheetCursor = (1,1),
    sheetOffset = (1,1),
    uiCols = 10,
    uiRows = 10,
    cWidth = 15,
    uiMode = ModeNormal
  }
