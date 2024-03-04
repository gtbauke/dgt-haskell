module Main (main) where

import Lib (createParserState, parseChar, runParser)

main :: IO ()
main = do
  let state = createParserState "hello"
  let result = runParser (parseChar 'h') state

  print result
