module Parse (parse) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as M

import Types
import Instructions (instructions)

type Parser = ParsecT String () Identity

parse :: FilePath -> String -> Either ParseError Mvanda
parse = P.parse mvanda

mvanda :: Parser Mvanda
mvanda = do
  l <- mvList'
  try $ spaces >> eof
  return l

mvValue :: Parser Mvanda
mvValue =
    mvNum
  <|> mvString
  <|> mvInstr
  <|> mvOp
  <|> mvList

mvNum :: Parser Mvanda
mvNum = (MvNum . read') <$> many1 digit
  where read' = toRational . (read :: String -> Integer)

mvString :: Parser Mvanda
mvString = do
  char '"'
  str <- many $ noneOf "\\\"" <|> strEscape
  char '"'
  return $ MvString str

mvOp :: Parser Mvanda
mvOp = oneOf "!#$%&'()*+,-./:;<=>?@\\^_`{|}~" >>= \x -> instr [x]

mvInstr :: Parser Mvanda
mvInstr = many1 lower >>= instr

mvList :: Parser Mvanda
mvList = do
  char '['
  vals <- mvList'
  spaces
  char ']'
  return vals

mvList' :: Parser Mvanda
mvList' = MvBlock <$> many1 (try $ spaces >> mvValue)

strEscape :: Parser Char
strEscape = do
  char '\\'
  ch <- anyChar
  return $ case ch of
    'n'  -> '\n'
    _    -> ch

instr :: String -> Parser Mvanda
instr s = case M.lookup s instructions of
  Nothing -> unexpected $ "invalid instruction `" ++ s ++ "`"
  Just i  -> return $ MvInstr i s