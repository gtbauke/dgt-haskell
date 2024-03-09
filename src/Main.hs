module Main (main) where

import Data.Bifunctor (Bifunctor (first))
import Lang (BinaryOperator (..), Expression (..))
import Lib (Parser (Parser, runParser), charParser, choice, oneOrMore, sequenceOf, zeroOrMore)
import System.Environment (getArgs)

intParser :: Parser Expression
intParser = Parser $ \input -> case reads input of
  [(x, rest)] -> [(IntegerExpression x, rest)]
  _ -> []

parseAddExpression' :: Parser Expression
parseAddExpression' = Parser $ \input -> do
  (int1, rest1) <- runParser intParser input
  (_, rest2) <- runParser (zeroOrMore whitespaceParser) rest1
  (_, rest3) <- runParser (charParser '+') rest2
  (_, rest4) <- runParser (zeroOrMore whitespaceParser) rest3
  (int2, _) <- runParser intParser rest4

  return (BinaryExpression Plus int1 int2, "")

-- addParser :: Parser Expression
-- addParser = BinaryExpression Plus <$> intParser <* zeroOrMore whitespaceParser <*> (charParser '+' <* zeroOrMore whitespaceParser *> intParser)

whitespaceParser :: Parser Char
whitespaceParser = choice (map charParser [' ', '\t'])

-- newlineParser :: Parser Char
-- newlineParser = choice (map charParser ['\n', '\r'])

-- identifierParser :: Parser Expression
-- identifierParser = IdentifierExpression <$> oneOrMore (choice (map charParser ['a' .. 'z'] ++ map charParser ['A' .. 'Z'] ++ [charParser '_']))

-- lineParser :: Parser Expression
-- lineParser = choice [addParser, identifierParser]

-- programParser :: Parser [Expression]
-- programParser = oneOrMore lineParser

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      putStrLn $ "The file name is: " ++ fileName
      file <- readFile fileName

      -- print $ runParser lineParser file
      -- print $ first generateIntExpression $ head $ runParser intParser file
      print $ runParser parseAddExpression' file
    _ -> do
      putStrLn "Usage: dtg <filename>"
