module Main where

import Sheet.Backend.Standard
import Sheet.Frontend.Types

import qualified Data.Map as M

main :: IO ()
main = do
  sh <- initUISheet
  c <- evalStateT test initSheet
  putStrLn (show c)

test :: StateT S IO (Maybe C)
test = do
  let p = (0,121120)
  setCell p (CellT "5 * 104" Nothing False)
  c <- getCell p
  case c of
    Just c_ -> liftIO $ do
                Just <$> evalStateT (evalCell c_) M.empty 
    Nothing -> return Nothing

initUISheet :: IO UISheet
initUISheet = do
  let cols = 7
      rows = 12
  return $ UISheet initSheet (0,0) (0,0)
