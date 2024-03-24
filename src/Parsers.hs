{-# LANGUAGE LambdaCase #-}

module Parsers
  ( charParser,
    sequenceOf,
    choice,
    zeroOrMore,
    oneOrMore,
    Parser (..),
    ParserState (..),
    ParserError (..),
    mapParser,
  )
where

import Control.Applicative (Alternative (..))

newtype Parser a = Parser {runParser :: String -> ParserState a}

data ParserState a
  = ParserError {_error :: ParserError, _input :: String}
  | ParserSuccess {_result :: a, _rest :: String}
  deriving (Show)

data ParserError
  = UnexpectedCharacter Char
  | UnexpectedEndOfInput
  | NoParsersToChooseFrom
  | FailedToParseInt
  | FailedToParseFloat
  | FailedToParseBool
  | FailedToParseExpression
  deriving (Show)

-- Parsers
charParser :: Char -> Parser Char
charParser expected = Parser $ \case
  (x : xs) ->
    if x == expected
      then ParserSuccess {_result = x, _rest = xs}
      else ParserError {_error = UnexpectedCharacter x, _input = xs}
  [] -> ParserError {_error = UnexpectedEndOfInput, _input = []}

-- Combinators
sequenceOf :: [Parser a] -> Parser [a]
sequenceOf [] = Parser $ \input -> ParserSuccess {_result = [], _rest = input}
sequenceOf (p : ps) = Parser $ \input -> case runParser p input of
  ParserError e i -> ParserError e i
  ParserSuccess r rest' -> case runParser (sequenceOf ps) rest' of
    ParserError e i -> ParserError e i
    ParserSuccess rs rest'' -> ParserSuccess (r : rs) rest''

choice :: [Parser a] -> Parser a
choice [] = Parser $ \input -> ParserError {_error = NoParsersToChooseFrom, _input = input}
choice (p : ps) = Parser $ \input -> case runParser p input of
  ParserError _ _ -> runParser (choice ps) input
  x -> x

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = Parser $ \input -> case runParser p input of
  ParserError _ _ -> ParserSuccess {_result = [], _rest = input}
  x -> case runParser (zeroOrMore p) (_rest x) of
    ParserError _ _ -> ParserSuccess {_result = [_result x], _rest = _rest x}
    ParserSuccess rs rest' -> ParserSuccess (_result x : rs) rest'

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = Parser $ \input -> case runParser p input of
  ParserError _ _ -> ParserError {_error = UnexpectedEndOfInput, _input = input}
  ParserSuccess r rest' -> case runParser (zeroOrMore p) rest' of
    ParserError _ _ -> ParserSuccess {_result = [r], _rest = rest'}
    ParserSuccess rs rest'' -> ParserSuccess (r : rs) rest''

-- Utility
mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f p = Parser $ \input -> case runParser p input of
  ParserError e i -> ParserError e i
  ParserSuccess r rest -> ParserSuccess (f r) rest

-- Functor
instance Functor ParserState where
  fmap _ (ParserError e i) = ParserError e i
  fmap f (ParserSuccess r rest) = ParserSuccess (f r) rest

instance Functor Parser where
  fmap = mapParser

-- Applicative
instance Applicative ParserState where
  pure x = ParserSuccess x []

  ParserError e i <*> _ = ParserError e i
  ParserSuccess f _ <*> px = fmap f px

instance Applicative Parser where
  pure x = Parser $ \input -> ParserSuccess x input

  pf <*> px = Parser $ \input -> case runParser pf input of
    ParserError e i -> ParserError e i
    ParserSuccess f rest -> runParser (fmap f px) rest

-- Monad
instance Monad ParserState where
  ParserError e i >>= _ = ParserError e i
  ParserSuccess r _ >>= f = f r

instance Monad Parser where
  p >>= f = Parser $ \input -> case runParser p input of
    ParserError e i -> ParserError e i
    ParserSuccess r rest -> runParser (f r) rest

-- Alternative
instance Alternative ParserState where
  empty = ParserError FailedToParseExpression ""

  ParserError _ _ <|> y = y
  x <|> _ = x

instance Alternative Parser where
  empty = Parser $ \input -> ParserError FailedToParseExpression input

  x <|> y = Parser $ \input -> case runParser x input of
    ParserError _ _ -> runParser y input
    x' -> x'

  many x = Parser $ \input -> case runParser x input of
    ParserError _ _ -> ParserSuccess [] input
    ParserSuccess r rest -> case runParser (many x) rest of
      ParserError _ _ -> ParserSuccess [r] rest
      ParserSuccess rs rest' -> ParserSuccess (r : rs) rest'

  some x = Parser $ \input -> case runParser x input of
    ParserError _ _ -> ParserError UnexpectedEndOfInput input
    ParserSuccess r rest -> case runParser (many x) rest of
      ParserError _ _ -> ParserSuccess [r] rest
      ParserSuccess rs rest' -> ParserSuccess (r : rs) rest'
