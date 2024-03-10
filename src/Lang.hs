module Lang (Expression (..)) where

data Expression
  = IntegerExpression Int
  | FloatExpression Float
  | StringExpression String
  | BooleanExpression Bool
  | Identifier String
  | UnaryExpression String Expression
  | BinaryExpression Expression String Expression
  deriving (Show)
