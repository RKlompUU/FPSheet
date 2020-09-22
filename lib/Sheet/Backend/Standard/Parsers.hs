module Sheet.Backend.Standard.Parsers where

import Control.Monad
import Data.Maybe
import Data.Char
import ParseLib.Simple hiding ((>>=))
import Prelude hiding ((<$>), (<*>), (<*), (*>))

import Sheet.Backend.Standard.Types


type SParser a = Parser Char a

parseCellDef :: String -> CellDef
parseCellDef str =
  let res = parse cellDefParser str
  in fst $ head $ res

cellDefParser :: SParser CellDef
cellDefParser =
      IODef  <$> (symbol '`' *> many anySymbol <* symbol '`')
  <|> Import <$> (token ":m" *> pWhitespace *> many anySymbol)
  <|> Load   <$> (token ":l" *> pWhitespace *> many anySymbol)
  <|> LanguageExtension <$> (token ":e" *> pWhitespace *> many anySymbol)
  <|> LetDef <$> greedy anySymbol

parsePos :: String -> Maybe Pos
parsePos str =
  let res = parse posParser str
  in if null res
      then Nothing
      else Just $ fst $ head $ res

posParser :: SParser Pos
posParser =
  (,) <$> pCol <*> pRow

-- Might write this better (if it can be done better) later.
-- For now a copy paste from:
--  https://stackoverflow.com/questions/40950853/excel-column-to-int-and-vice-versa-improvements-sought
--
-- given a spreadsheet column as a string
-- returns integer giving the position of the column
-- ex:
-- toInt "A" = 1
-- toInt "XFD" = 16384
toInt :: String -> Int
toInt = foldl fn 0
  where
    fn = \a c -> 26*a + ((ord c)-(ord 'a' - 1))

toCol :: Int -> String
toCol c = ([0..] >>= flip replicateM ['a'..'z']) !! c

pCol :: SParser Int
pCol =
  toInt <$> some (satisfy (\c -> any (== c) ['a'..'z']))
pRow :: SParser Int
pRow =
  natural

pWhitespace =
  let wspace = [' ','\t','\n','\r']
  in const () <$> greedy1 (satisfy (flip elem wspace))

