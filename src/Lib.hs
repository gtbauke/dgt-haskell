module Lib
  ( runParser,
    createParserState,
    parseChar,
    parseString,
  )
where

data ParserState i a
  = ParserState {sData :: i, sConsumedData :: i, sResult :: a, sIndex :: Int}
  deriving (Show)

newtype Parser i a e = Parser {runParser :: ParserState i a -> Either e (ParserState i a)}

createParserState :: i -> ParserState i a
createParserState i = ParserState i i undefined 0

parseChar :: Char -> Parser String Char String
parseChar expected = Parser $ \state ->
  let input = sConsumedData state
   in case input of
        [] -> Left "No more input"
        (c : cs) ->
          if c == expected
            then Right $ state {sConsumedData = cs, sResult = c, sIndex = sIndex state + 1}
            else Left $ "Expected " ++ [expected] ++ " but got " ++ [c]

parseString :: String -> Parser String String String
parseString expected = Parser $ \state ->
  let input = sConsumedData state
      len = length expected
   in if take len input == expected
        then Right $ state {sConsumedData = drop len input, sResult = expected, sIndex = sIndex state + len}
        else Left $ "Expected " ++ expected ++ " but got " ++ take len input
