module Main where

import Sheet.Backend.API
import Sheet.Frontend.Types

main :: IO ()
main = do
  sh <- initUISheet
  let p = (0,121120)
  let c = getText <$> evalState (setCell p (CellT "Test string" Nothing False) >> getCell p) (sheetCells sh)
  putStrLn (show c)

initUISheet :: IO UISheet
initUISheet = do
  let cols = 7
      rows = 12
  return $ UISheet initSheet (0,0) (0,0)
