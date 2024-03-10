{-# LANGUAGE LambdaCase #-}

module Parsers
  ( charParser,
    stringParser,
    sequenceOf,
    choice,
    zeroOrMore,
    oneOrMore,
    anyParser,
    Parser (..),
    ParserState (..),
  )
where

import Data.List (isPrefixOf)

data ParserState a = ParserState {result :: a, rest :: String} | ParserError String
  deriving (Show)

newtype Parser a = Parser {runParser :: String -> ParserState a}

-- !: useful resource -> https://serokell.io/blog/parser-combinators-in-haskell

{-
-- TODO: explain the Functor, Applicative, Monad and Alternative? instances in the tutorial text
-- TODO: I don't think that all these implementations are correct. I need to check them again.
instance Functor ParserState where
  fmap f (ParserState a rest) = ParserState (f a) rest
  fmap _ (ParserError e) = ParserError e

instance Applicative ParserState where
  pure a = ParserState a ""

  ParserState f _ <*> ParserState a rest' = ParserState (f a) rest'
  ParserError e <*> _ = ParserError e
  _ <*> ParserError e = ParserError e

instance Monad ParserState where
  return = pure

  ParserState a _ >>= f = f a
  ParserError e >>= _ = ParserError e

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> case p input of
    ParserState a rest' -> ParserState (f a) rest'
    ParserError e -> ParserError e

instance Applicative Parser where
  pure a = Parser $ \input -> ParserState a input
  Parser f <*> Parser a = Parser $ \input -> case f input of
    ParserState f' rest' -> case a rest' of
      ParserState a' rest'' -> ParserState (f' a') rest''
      ParserError e -> ParserError e
    ParserError e -> ParserError e

instance Monad Parser where
  return = pure
  Parser a >>= f = Parser $ \input -> case a input of
    ParserState a' rest' -> runParser (f a') rest'
    ParserError e -> ParserError e
-}

charParser :: Char -> Parser Char
charParser e = Parser $ \case
  (x : xs) -> if x == e then ParserState {result = x, rest = xs} else ParserError "Unexpected character"
  [] -> ParserError "Unexpected end of input"

stringParser :: String -> Parser String
stringParser expected = Parser $ \input ->
  if expected `isPrefixOf` input
    then ParserState {result = expected, rest = drop (length expected) input}
    else ParserError "Unexpected string"

{-
stringParser' :: String -> Parser String
stringParser' = traverse charParser
-}

sequenceOf :: [Parser a] -> Parser [a]
sequenceOf [] = Parser $ \input -> ParserState {result = [], rest = input}
sequenceOf (p : ps) = Parser $ \input -> case runParser p input of
  ParserError e -> ParserError e
  ParserState {result = r, rest = rest'} -> case runParser (sequenceOf ps) rest' of
    ParserError e -> ParserError e
    ParserState {result = rs, rest = rest''} -> ParserState {result = r : rs, rest = rest''}

{-
sequenceOf' :: [Parser a] -> Parser [a]
sequenceOf' [] = Parser $ \input -> ParserState {result = [], rest = input}
sequenceOf' (p : ps) = do
  r <- p
  rs <- sequenceOf' ps
  return (r : rs)
-}

choice :: [Parser a] -> Parser a
choice [] = Parser $ \_ -> ParserError "No parsers to choose from"
choice (p : ps) = Parser $ \input -> case runParser p input of
  ParserError _ -> runParser (choice ps) input
  x -> x

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = Parser $ \input -> case runParser p input of
  ParserError _ -> ParserState {result = [], rest = input}
  x -> case runParser (zeroOrMore p) (rest x) of
    ParserError _ -> ParserState {result = [result x], rest = rest x}
    ParserState {result = rs, rest = rest'} -> ParserState {result = result x : rs, rest = rest'}

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = Parser $ \input -> case runParser p input of
  ParserError _ -> ParserError "Expected one or more"
  ParserState {result = r, rest = rest'} -> case runParser (zeroOrMore p) rest' of
    ParserError _ -> ParserState {result = [r], rest = rest'}
    ParserState {result = rs, rest = rest''} -> ParserState {result = r : rs, rest = rest''}

-- TODO: explain this parser in the tutorial text

anyParser :: Parser Char
anyParser = Parser $ \case
  (x : xs) -> ParserState {result = x, rest = xs}
  [] -> ParserError "Unexpected end of input"
