module Main (main) where

import Lang (BinaryOperator (..), Expression (..))
import Lib (Parser (runParser), charParser, choice, oneOrMore, sequenceOf, zeroOrMore)
import System.Environment (getArgs)

intParser :: Parser Expression
intParser = IntegerExpression . read <$> oneOrMore (choice (map charParser ['0' .. '9']))

addParser :: Parser Expression
addParser = BinaryExpression Plus <$> intParser <* zeroOrMore whitespaceParser <*> (charParser '+' <* zeroOrMore whitespaceParser *> intParser)

whitespaceParser :: Parser Char
whitespaceParser = choice (map charParser [' ', '\t'])

newlineParser :: Parser Char
newlineParser = choice (map charParser ['\n', '\r'])

identifierParser :: Parser Expression
identifierParser = IdentifierExpression <$> oneOrMore (choice (map charParser ['a' .. 'z'] ++ map charParser ['A' .. 'Z'] ++ [charParser '_']))

lineParser :: Parser Expression
lineParser = choice [addParser, identifierParser]

programParser :: Parser [Expression]
programParser = oneOrMore lineParser

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      putStrLn $ "The file name is: " ++ fileName
      file <- readFile fileName

      print $ runParser lineParser file
    _ -> do
      putStrLn "Usage: dtg <filename>"
