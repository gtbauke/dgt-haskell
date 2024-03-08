module Lang (Expression (..), BinaryOperator (..)) where

data BinaryOperator
  = Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | Power
  | Equal
  | NotEqual
  | Less
  | LessOrEqual
  | Greater
  | GreaterOrEqual
  | And
  | Or
  deriving (Show)

data Expression
  = IntegerExpression Int
  | FloatExpression Float
  | IdentifierExpression String
  | BinaryExpression BinaryOperator Expression Expression
  deriving (Show)
