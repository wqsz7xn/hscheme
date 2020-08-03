module Scheme where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Eq, Show)

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseNumber' :: Parser LispVal
parseNumber' = do
  xs <- many1 digit
  return $ (Number . read) xs

parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= (\xs -> return $ (Number . read) xs)

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "?" input of
  Left err -> "No match " <> show err
  Right val -> "Found value: " <> show val

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr
