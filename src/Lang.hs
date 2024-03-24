module Lang
  ( Expression (..),
    UnaryOp (..),
    BinaryOp (..),
    prettyAST,
    intParser,
    floatParser,
    boolParser,
    whitespaceParser,
    expressionParser,
    binaryExpressionParser,
    intParser',
    floatParser',
    floatParser'',
    boolParser',
  )
where

import Control.Applicative (Alternative (..))
import Parsers
  ( Parser (..),
    ParserError (FailedToParseBool, FailedToParseExpression, FailedToParseFloat, FailedToParseInt),
    ParserState
      ( ParserError,
        ParserSuccess,
        _error,
        _input,
        _rest,
        _result
      ),
    charParser,
    choice,
    mapParser,
    oneOrMore,
    sequenceOf,
    zeroOrMore,
  )

data UnaryOp
  = Negate -- Operação de negação
  | Not -- Operação de negação lógica
  deriving (Show)

data BinaryOp
  = Add -- Operação de adição
  | Sub -- Operação de subtração
  | Mul -- Operação de multiplicação
  | Div -- Operação de divisão
  | And -- Operação de conjunção
  | Or -- Operação de disjunção
  | Eq -- Operação de igualdade
  | Neq -- Operação de desigualdade
  | Lt -- Operação de menor que
  | Lte -- Operação de menor ou igual a
  | Gt -- Operação de maior que
  | Gte -- Operação de maior ou igual a
  deriving (Show)

fromString :: String -> BinaryOp
fromString "+" = Add
fromString "-" = Sub
fromString "*" = Mul
fromString "/" = Div
fromString "&&" = And
fromString "||" = Or
fromString "==" = Eq
fromString "!=" = Neq
fromString "<" = Lt
fromString "<=" = Lte
fromString ">" = Gt
fromString ">=" = Gte
fromString _ = error "Invalid binary operator"

data Expression
  = ExprInt Int
  | ExprFloat Float
  | ExprBool Bool
  | ExprUnary UnaryOp Expression
  | ExprBinary BinaryOp Expression Expression
  | ExprIf Expression Expression Expression
  deriving (Show)

prettyAST :: Expression -> String
prettyAST = prettyAST' 0
  where
    prettyAST' :: Int -> Expression -> String
    prettyAST' indent (ExprInt n) = replicate indent ' ' ++ show n
    prettyAST' indent (ExprFloat n) = replicate indent ' ' ++ show n
    prettyAST' indent (ExprBool b) = replicate indent ' ' ++ show b
    prettyAST' indent (ExprUnary op e) = replicate indent ' ' ++ show op ++ " " ++ prettyAST' 0 e
    prettyAST' indent (ExprBinary op e1 e2) =
      replicate indent ' '
        ++ prettyAST' 0 e1
        ++ " "
        ++ show op
        ++ " "
        ++ prettyAST' 0 e2
    prettyAST' indent (ExprIf e1 e2 e3) =
      replicate indent ' '
        ++ "if\n"
        ++ prettyAST' (indent + 2) e1
        ++ "\n"
        ++ replicate indent ' '
        ++ "then\n"
        ++ prettyAST' (indent + 2) e2
        ++ "\n"
        ++ replicate indent ' '
        ++ "else\n"
        ++ prettyAST' (indent + 2) e3

intParser :: Parser Expression
intParser = Parser $ \input -> case runParser (oneOrMore (choice (map charParser ['0' .. '9']))) input of
  ParserError _ _ -> ParserError {_error = FailedToParseInt, _input = input}
  ParserSuccess digits rest -> ParserSuccess {_result = ExprInt (read digits), _rest = rest}

floatParser :: Parser Expression
floatParser = Parser $ \input -> case runParser (oneOrMore (choice (map charParser ['0' .. '9']))) input of
  ParserError _ _ -> ParserError {_error = FailedToParseFloat, _input = input}
  ParserSuccess digits rest -> case runParser (charParser '.') rest of
    ParserError _ _ -> ParserError {_error = FailedToParseFloat, _input = rest}
    ParserSuccess _ rest' -> case runParser (oneOrMore (choice (map charParser ['0' .. '9']))) rest' of
      ParserError _ _ -> ParserError {_error = FailedToParseFloat, _input = rest'}
      ParserSuccess digits' rest'' -> ParserSuccess {_result = ExprFloat (read (digits ++ "." ++ digits')), _rest = rest''}

boolParser :: Parser Expression
boolParser = Parser $ \input -> case runParser (choice [trueParser, falseParser]) input of
  ParserError _ _ -> ParserError {_error = FailedToParseBool, _input = input}
  ParserSuccess result rest -> ParserSuccess {_result = ExprBool (result == "true"), _rest = rest}
  where
    trueParser = sequenceOf (map charParser "true")
    falseParser = sequenceOf (map charParser "false")

binaryExpressionParser :: Parser Expression
binaryExpressionParser = Parser $ \input -> case runParser expressionParser (clean input) of
  ParserError _ _ -> ParserError {_error = FailedToParseExpression, _input = input}
  ParserSuccess expr1 rest -> case runParser binaryOperator (clean rest) of
    ParserError _ _ -> ParserError {_error = FailedToParseExpression, _input = clean rest}
    ParserSuccess op rest' -> case runParser expressionParser (clean rest') of
      ParserError _ _ -> ParserError {_error = FailedToParseExpression, _input = clean rest'}
      ParserSuccess expr2 rest'' -> ParserSuccess {_result = ExprBinary (fromString op) expr1 expr2, _rest = rest''}
  where
    stringParser = sequenceOf . map charParser
    binaryOperator = choice (map stringParser ["+", "-", "*", "/", "&&", "||", "==", "!=", "<", "<=", ">", ">="])
    clean val = case runParser whitespaceParser val of
      ParserError _ _ -> val
      ParserSuccess _ rest' -> rest'

expressionParser :: Parser Expression
expressionParser = choice [floatParser, intParser, boolParser, binaryExpressionParser]

whitespaceParser :: Parser String
whitespaceParser = zeroOrMore (choice [charParser ' ', charParser '\n', charParser '\t'])

intParser' :: Parser Expression
intParser' = ExprInt <$> fmap (read :: String -> Int) (oneOrMore (choice (map charParser ['0' .. '9'])))

floatParser' :: Parser Expression
floatParser' =
  ExprFloat <$> do
    digits <- oneOrMore (choice (map charParser ['0' .. '9']))
    _ <- charParser '.'
    digits' <- oneOrMore (choice (map charParser ['0' .. '9']))

    return (read (digits ++ "." ++ digits'))

floatParser'' :: Parser Expression
floatParser'' =
  ExprFloat
    <$> ( oneOrMore (choice (map charParser ['0' .. '9']))
            >>= \digits ->
              charParser '.'
                >> ( oneOrMore (choice (map charParser ['0' .. '9']))
                       >>= \digits' -> return (read (digits ++ "." ++ digits'))
                   )
        )

boolParser' :: Parser Expression
boolParser' = ExprBool <$> fmap (== "true") (sequenceOf (map charParser "true") <|> sequenceOf (map charParser "false"))
