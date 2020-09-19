module Sheet.Frontend.CmdParser where

import Prelude hiding ((<*>), (<|>), (<$>), (<*), (*>), (<$), ($>))
import ParseLib

import Sheet.Backend.Standard

data Command =
  CmdInvalid |
  CmdMoveCursor Int Int |
  CmdImport Bool String |
  CmdSaveActiveFile |
  CmdSave String |
  CmdLoad String |
  CmdQuit Bool

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
  CmdSaveActiveFile <$ symbol 'w' <|>
  CmdSave <$> (symbol 'w' *> pWhitespace *> pFilename) <|>
  CmdLoad <$> (symbol 'r' *> pWhitespace *> pFilename) <|>
  CmdQuit True <$ token "wq" <|>
  CmdQuit False <$ symbol 'q'

pFilename :: CmdParser String
pFilename =
  let fchars = ['a'..'z'] ++ ['A'..'Z'] ++ ['.', '/']
  in greedy1 (satisfy (flip elem fchars))

