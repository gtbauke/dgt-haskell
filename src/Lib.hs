module Lib
  ( createParserState,
    parseChar,
    runParser,
  )
where

data ParserState i a e
  = ParserState {sData :: i, sResult :: a, sIndex :: Int}
  | ParserError {sData :: i, sError :: e, sIndex :: Int}
  deriving (Show)

newtype Parser i a e = Parser {runParser :: ParserState i a e -> ParserState i a e}

createParserState :: i -> ParserState i a e
createParserState input = ParserState {sData = input, sResult = undefined, sIndex = 0}

parseChar :: Char -> Parser String Char String
parseChar target = Parser $ \state ->
  case sData state of
    [] -> ParserError {sData = sData state, sError = "End of input", sIndex = sIndex state}
    (c : cs) ->
      if c == target
        then ParserState {sData = cs, sResult = c, sIndex = sIndex state + 1}
        else ParserError {sData = sData state, sError = "Expected " ++ [target], sIndex = sIndex state}
