{-# LANGUAGE OverloadedStrings #-}
module Sheet.Backend.Standard.Saves where

import Sheet.Backend.Standard.Types

import qualified Data.ByteString.Lazy as L
import Codec.Xlsx
--import Codec.Xlsx.Types

import Control.Lens
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Tuple

import Control.Applicative

import Data.Aeson
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T


saveSheet :: Sheet C -> String -> IO ()
saveSheet s f = do
  let save = Save (s_cells s)
  T.writeFile f $ T.decodeUtf8 . encode $ save


loadSheet :: String -> IO (Maybe Save)
loadSheet f = do
  decode . T.encodeUtf8 <$> T.readFile f


importCells :: String -> Bool -> IO (M.Map Pos C)
importCells f simpleImport = do
  xlsxC <- loadXlsxCells f
  return
    $ M.mapKeys swap
    $ M.mapWithKey (fromXlsxCell simpleImport) xlsxC


loadXlsxCells :: String -> IO CellMap
loadXlsxCells f = do
  bs <- L.readFile f
  let xlsx = toXlsx bs
      mainSheet :: Worksheet
      mainSheet = snd $ head $ xlsx ^. xlSheets
  return $ mainSheet ^. wsCells


fromXlsxCell :: Bool -> Pos -> Cell -> C
fromXlsxCell simpleImport (row, col) cell =
  let content = (if simpleImport
                  then empty
                  else unCellExpression <$> _cellfExpression <$> cell ^. cellFormula)
            <|> unCellValue <$> cell ^. cellValue
  in CellT {
    c_def = maybe (LetDef "failed") LetDef content,
    c_res = Nothing,
    c_uFlag = False,
    c_pos = (col, row)
  }


unCellValue :: CellValue -> String
unCellValue (CellText x) =
  T.unpack x
unCellValue (CellDouble x) =
  show x
unCellValue (CellBool x) =
  show x
unCellValue (CellRich xs) =
  show xs
unCellValue (CellError err) =
  show err


unCellExpression :: FormulaExpression -> String
unCellExpression (NormalFormula formula) =
  T.unpack $ unFormula formula
unCellExpression (SharedFormula formula) =
  unCellExpression $ _cellfExpression $ sharedFormulaByIndex formula
