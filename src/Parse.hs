module Parse (parse) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P
import Data.Functor.Identity (Identity)

import Types

type Parser = ParsecT String () Identity

parse :: FilePath -> String -> Either ParseError Mvanda
parse = P.parse $ do
  l <- mvList'
  spaces
  return l

mvValue :: Parser Mvanda
mvValue = spaces >> mvValue'

mvValue' :: Parser Mvanda
mvValue' =
    mvNum
  <|> mvString
  <|> mvAtom
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
mvOp = (MvAtom . return) <$> oneOf "!#$%&'()*+,-./:;<=>?@\\^_`{|}~"

mvAtom :: Parser Mvanda
mvAtom = MvAtom <$> many1 lower

mvList :: Parser Mvanda
mvList = do
  char '['
  vals <- mvList'
  spaces
  char ']'
  return vals

mvList' :: Parser Mvanda
mvList' = MvList <$> many (try mvValue)

strEscape :: Parser Char
strEscape = do
  char '\\'
  ch <- anyChar
  return $ case ch of
    'n'  -> '\n'
    _    -> ch
