{-# LANGUAGE LambdaCase #-}

module Lib
  ( charParser,
    stringParser,
    sequenceOf,
    choice,
    zeroOrMore,
    oneOrMore,
  )
where

import Data.List (isPrefixOf)

data ParserState a = ParserState {result :: a, rest :: String} | ParserError String
  deriving (Show)

newtype Parser a = Parser {runParser :: String -> ParserState a}

charParser :: Char -> Parser Char
charParser e = Parser $ \case
  (x : xs) -> if x == e then ParserState {result = x, rest = xs} else ParserError "Unexpected character"
  [] -> ParserError "Unexpected end of input"

stringParser :: String -> Parser String
stringParser expected = Parser $ \input ->
  if expected `isPrefixOf` input
    then ParserState {result = expected, rest = drop (length expected) input}
    else ParserError "Unexpected string"

sequenceOf :: [Parser a] -> Parser [a]
sequenceOf [] = Parser $ \input -> ParserState {result = [], rest = input}
sequenceOf (p : ps) = Parser $ \input -> case runParser p input of
  ParserError e -> ParserError e
  ParserState {result = r, rest = rest'} -> case runParser (sequenceOf ps) rest' of
    ParserError e -> ParserError e
    ParserState {result = rs, rest = rest''} -> ParserState {result = r : rs, rest = rest''}

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
