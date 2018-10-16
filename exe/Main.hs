module Main where

import Sheet.Backend.Standard
import Sheet.Frontend.Types

import qualified Data.Map as M

main :: IO ()
main = do
  sh <- initUISheet
  c <- evalStateT test initSheet
  putStrLn (show c)

test :: StateT S IO C
test = do
  let p = (0,121120)
  setCell ((newCell p) {cStr = "(\\x -> x) $ 5 * 104"})
  getCell p >>= evalCell
  getCell p

initUISheet :: IO UISheet
initUISheet = do
  let cols = 7
      rows = 12
  return $ UISheet initSheet (0,0) (0,0)
