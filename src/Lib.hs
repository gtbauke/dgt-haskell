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

-- sequenceOf :: [Parser i a e] -> Parser i [a] e
-- sequenceOf [] = Parser $ \state -> Right $ state {sResult = [], sIndex = sIndex state}
-- sequenceOf (p:ps) = Parser $ \state -> case runParser p state of
--   Left e -> Left e
--   Right newState -> case runParser (sequenceOf ps) newState of
--     Left e -> Left e
--     Right finalState -> Right $ finalState {sResult = sResult newState : sResult finalState}

parseString :: String -> Parser String String String
parseString expected = Parser $ \state ->
  let input = sConsumedData state
      len = length expected
   in if take len input == expected
        then Right $ state {sConsumedData = drop len input, sResult = expected, sIndex = sIndex state + len}
        else Left $ "Expected " ++ expected ++ " but got " ++ take len input
