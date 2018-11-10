module Main where

import Sheet.Backend.Standard
import Sheet.Frontend.Types
import Sheet.Frontend.TUI

import qualified Language.Haskell.Interpreter as I
import qualified Data.Map as M

main :: IO ()
main = do
  (c,s) <- runStateT test initSheet
  putStrLn (show c)
  runTUI (UISheet s (0,0) (0,0))

test :: StateTy C
test = do
  let p0 = (1,5)

  let p = (1,4)
  getCell p >>= setText "(\\x -> x + a5) $ 5 * 104" >>= evalCell

  getCell p0 >>= setText "5 * 3" >>= evalCell

  getCell p
