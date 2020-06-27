module Sheet.Frontend.Types where

import Sheet.Backend.Standard

import Brick.BChan
import Brick.Widgets.Edit

import qualified Data.Map as M

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

    screenWidth :: Int,
    screenHeight :: Int,

    uiMode :: UIMode,

    cellStatus :: M.Map Pos CellStatus,
    cellDecorations :: M.Map Pos CellDecorations,

    custEvChan :: BChan CustomEvent,
    showCellFeedbackTimeout :: Int -- in milliseconds
  }

data CellDecorations =
  CellDecorations {
    dec_underline :: Bool
  }

defaultCellDecorations =
  CellDecorations {
    dec_underline = False
  }

data UIMode =
    ModeNormal
  | ModeVisual
  | ModeEdit {
      cellEditor :: Editor String String,
      cellEditorWidth :: Int
    }
  | ModeCommand {
      cmdEditor :: Editor String String,
      cmdEditorWidth :: Int
  }

data CustomEvent =
  EvNewDefinition BackendJobResponse |
  EvVisualFeedback C CellStatus

initUISheet :: BChan CustomEvent -> IO UISheet
initUISheet customEvChan = do
  sheet <- initSheet (\j -> writeBChan customEvChan $ EvNewDefinition j)
                     (\cell stat -> writeBChan customEvChan $ EvVisualFeedback cell stat)
  return $ UISheet {
    sheetCells = sheet,
    sheetCursor = (1,1),
    sheetOffset = (1,1),
    uiCols = 10,
    uiRows = 10,
    cWidth = 15,
    screenWidth = 80,
    screenHeight = 24,
    uiMode = ModeNormal,
    cellStatus = M.empty,
    cellDecorations = M.empty,
    custEvChan = customEvChan,
    showCellFeedbackTimeout = 500
  }
