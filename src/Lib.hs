{-# LANGUAGE LambdaCase #-}

module Lib
  ( charParser,
    sequenceOf,
    choice,
    zeroOrMore,
    oneOrMore,
    Parser (..),
    mapResult,
  )
where

import Data.Bifunctor (first)

newtype Parser a = Parser {runParser :: String -> [(a, String)]}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> map (first f) (p input)

instance Applicative Parser where
  pure x = Parser $ \input -> [(x, input)]
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (f, input') <- p1 input
    (x, input'') <- p2 input'
    return (f x, input'')

charParser :: Char -> Parser Char
charParser expected = Parser $ \case
  [] -> []
  (x : xs) -> ([(x, xs) | x == expected])

sequenceOf :: [Parser a] -> Parser [a]
sequenceOf [] = Parser $ \input -> [([], input)]
sequenceOf (p : ps) = Parser $ \input ->
  case runParser p input of
    [(x, input')] -> case runParser (sequenceOf ps) input' of
      [(xs, input'')] -> [(x : xs, input'')]
      _ -> []
    _ -> []

choice :: [Parser a] -> Parser a
choice [] = Parser $ const []
choice (p : ps) = Parser $ \input ->
  case runParser p input of
    [] -> runParser (choice ps) input
    res -> res

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = Parser $ \input ->
  case runParser (oneOrMore p) input of
    [] -> [([], input)]
    res -> res

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = Parser $ \input ->
  case runParser p input of
    [(x, input')] -> case runParser (zeroOrMore p) input' of
      [(xs, input'')] -> [(x : xs, input'')]
      _ -> [([x], input')]
    _ -> []

mapResult :: (a -> b) -> Parser a -> Parser b
mapResult f (Parser p) = Parser $ \input -> map (first f) (p input)
