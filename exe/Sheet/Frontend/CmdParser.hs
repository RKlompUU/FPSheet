module Sheet.Frontend.CmdParser where

import Prelude hiding ((<*>), (<|>), (<$>), (<*), (*>), (<$), ($>))
import ParseLib

import Sheet.Backend.Standard

data Command =
  CmdInvalid |
  CmdMoveCursor Int Int

type CmdParser a = Parser Char a

parseCmd :: String -> Command
parseCmd cmd =
  case filter (null . snd) (parse cmdParser cmd) of
    [] -> CmdInvalid
    ((x,_):_) -> x

cmdParser :: CmdParser Command
cmdParser =
  CmdMoveCursor <$> pCol <*> pRow
