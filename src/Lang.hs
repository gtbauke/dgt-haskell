module Lang (Expression (..)) where

import Parsers (Parser (..), ParserState (..), anyParser, charParser, choice, oneOrMore, sequenceOf, stringParser, zeroOrMore)

data Expression
  = IntegerExpression Int
  | FloatExpression Float
  | StringExpression String
  | BooleanExpression Bool
  | Identifier String
  | UnaryExpression String Expression
  | BinaryExpression Expression String Expression
  | IfExpression {condition :: Expression, trueBranch :: Expression, falseBranch :: Expression}
  deriving (Show)

intExpressionParser :: Parser Expression
intExpressionParser = Parser $ \input ->
  case runParser (oneOrMore (choice (map charParser ['0' .. '9']))) input of
    ParserError _ -> ParserError "Expected integer"
    ParserState {result = r, rest = rest'} -> ParserState {result = IntegerExpression (read r), rest = rest'}

floatExpressionParser :: Parser Expression
floatExpressionParser = Parser $ \input -> case runParser (oneOrMore (choice (map charParser ['0' .. '9']))) input of
  ParserError _ -> ParserError "Expected Float"
  ParserState {result = r, rest = rest'} -> case runParser (charParser '.') rest' of
    ParserError _ -> ParserError "Expected Float"
    ParserState {rest = rest''} -> case runParser (oneOrMore (choice (map charParser ['0' .. '9']))) rest'' of
      ParserError _ -> ParserError "Expected Float"
      ParserState {result = r', rest = rest'''} -> ParserState {result = FloatExpression (read (r ++ "." ++ r')), rest = rest'''}

stringExpressionParser :: Parser Expression
stringExpressionParser = Parser $ \input -> case runParser (charParser '"') input of
  ParserError _ -> ParserError "Expected string"
  ParserState {rest = rest'} -> case runParser (zeroOrMore anyParser) rest' of
    ParserError _ -> ParserError "Expected string"
    ParserState {result = r, rest = rest''} -> case runParser (charParser '"') rest'' of
      ParserError _ -> ParserError "Expected string"
      ParserState {rest = rest'''} -> ParserState {result = StringExpression r, rest = rest'''}

booleanExpressionParser :: Parser Expression
booleanExpressionParser = Parser $ \input -> case runParser (choice [stringParser "true", stringParser "false"]) input of
  ParserError _ -> ParserError "Expected boolean"
  ParserState {result = r, rest = rest'} -> ParserState {result = BooleanExpression (r == "true"), rest = rest'}

identifierExpressionParser :: Parser Expression
identifierExpressionParser = Parser $ \input -> case runParser (oneOrMore (choice (map charParser (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_'])))) input of
  ParserError _ -> ParserError "Expected identifier"
  ParserState {result = r, rest = rest'} -> ParserState {result = Identifier r, rest = rest'}

-- TODO: Implement the rest of the parsers
