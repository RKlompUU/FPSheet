module Main where

import Sheet.Backend.Standard
import Sheet.Frontend.Types

import qualified Language.Haskell.Interpreter as I
import qualified Data.Map as M

main :: IO ()
main = do
  sh <- initUISheet
  c <- I.runInterpreter (I.setImports ["Prelude"] >> evalStateT test initSheet)
  putStrLn (show c)

test :: StateTy C
test = do
  let p0 = (1,5)
  setCell ((newCell p0) {c_str = "5 * 3"})
  getCell p0 >>= evalCell

  let p = (1,4)
  setCell ((newCell p) {c_str = "(\\x -> x + a5) $ 5 * 104"})
  getCell p >>= evalCell
  getCell p

initUISheet :: IO UISheet
initUISheet = do
  let cols = 7
      rows = 12
  return $ UISheet initSheet (0,0) (0,0)
