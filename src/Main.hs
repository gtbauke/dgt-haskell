module Main (main) where

import Lang (BinaryOperator (..), Expression (..))
import Lib (Parser (runParser), charParser, choice, oneOrMore, sequenceOf, zeroOrMore)
import System.Environment (getArgs)

intParser :: Parser Expression
intParser = IntegerExpression . read <$> oneOrMore (choice (map charParser ['0' .. '9']))

addParser :: Parser Expression
addParser = BinaryExpression Plus <$> intParser <* zeroOrMore (charParser ' ') <*> (charParser '+' <* zeroOrMore (charParser ' ') *> intParser)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      putStrLn $ "The file name is: " ++ fileName
      file <- readFile fileName

      print $ runParser intParser file
      print $ runParser addParser file
    _ -> do
      putStrLn "Usage: dtg <filename>"
