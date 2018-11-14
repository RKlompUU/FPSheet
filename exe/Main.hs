module Main where

import Sheet.Backend.Standard
import Sheet.Frontend.Types
import Sheet.Frontend.TUI

import qualified Language.Haskell.Interpreter as I
import qualified Data.Map as M

import System.Console.Terminal.Size

main :: IO ()
main = do
  (c,s) <- runStateT test initSheet
  (cols, rows) <- maybe (80,24) (\w -> (width w, height w))
              <$> size
  runTUI $ uiResize cols rows (initUISheet { sheetCells = s })

test :: StateTy C
test = do
  let p0 = (3,4)

  let p = (1,4)
  getCell p >>= setText "(\\x -> x + c4) $ 5 * 104" >>= evalCell

  getCell (1,5) >>= setText "\"pretty long string here\"" >>= evalCell
  getCell (3,5) >>= setText "\"also pretty long\"" >>= evalCell

  --getCell p0 >>= setText "5 * 3" >>= evalCell

  getCell p
