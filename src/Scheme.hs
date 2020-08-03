module Scheme where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

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

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "?" input of
  Left err -> "No match " <> show err
  Right val -> "Found value " 

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Hello, " <> (args !! 0) <> " " <> (args !! 1)
  putStrLn $ show (read "123" :: Int )
  line <- getLine
  putStrLn $ "Hello, " <> line
  main'

main' :: IO ()
main' = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr
