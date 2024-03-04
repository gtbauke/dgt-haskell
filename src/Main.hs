module Main (main) where

import Lib (createParserState, parseChar, parseString, runParser)

main :: IO ()
main = do
  let state = createParserState "hellow"
  let result = runParser (parseString "hello") state

  print result
