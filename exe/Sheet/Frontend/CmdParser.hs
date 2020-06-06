module Sheet.Frontend.CmdParser where

import Prelude hiding ((<*>), (<|>), (<$>), (<*), (*>), (<$), ($>))
import ParseLib

import Sheet.Backend.Standard

data Command =
  CmdInvalid |
  CmdMoveCursor Int Int |
  CmdImport Bool String |
  CmdSave String |
  CmdQuit

type CmdParser a = Parser Char a

parseCmd :: String -> Command
parseCmd cmd =
  case filter (null . snd) (parse cmdParser cmd) of
    [] -> CmdInvalid
    ((x,_):_) -> x

cmdParser :: CmdParser Command
cmdParser =
  CmdMoveCursor <$> pCol <*> pRow <|>
  CmdImport <$> ((=='i') <$> (symbol 'i' <|> symbol 'I')) <*> (pWhitespace *> pFilename) <|>
  CmdSave <$> (symbol 'w' *> pWhitespace *> pFilename) <|>
  CmdQuit <$ symbol 'q'

pWhitespace :: CmdParser ()
pWhitespace =
  let wspace = [' ','\t','\n','\r']
  in const () <$> greedy (satisfy (flip elem wspace))

pFilename :: CmdParser String
pFilename =
  let fchars = ['a'..'z'] ++ ['A'..'Z'] ++ ['.', '/']
  in greedy (satisfy (flip elem fchars))

