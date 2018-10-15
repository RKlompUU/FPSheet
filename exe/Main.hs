module Main where

import Sheet.Backend.API
import Sheet.Frontend.Types

main :: IO ()
main =
  putStrLn "This executable is not doing anything just yet!"

initUISheet :: IO UISheet
initUISheet = do
  let cols = 7
      rows = 12
  return $ UISheet initSheet (0,0) (0,0)
