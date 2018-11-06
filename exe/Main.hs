module Main where

import Sheet.Backend.Standard
import Sheet.Frontend.Types

import qualified Language.Haskell.Interpreter as I
import qualified Data.Map as M

main :: IO ()
main = do
  sh <- initUISheet
  c <- evalStateT test initSheet
  putStrLn (show c)

test :: StateTy C
test = do
  let p0 = (1,5)

  let p = (1,4)
  getCell p >>= setText "(\\x -> x + a5) $ 5 * 104" >>= evalCell

  getCell p0 >>= setText "5 * 3" >>= evalCell

  getCell p

initUISheet :: IO UISheet
initUISheet = do
  let cols = 7
      rows = 12
  return $ UISheet initSheet (0,0) (0,0)
