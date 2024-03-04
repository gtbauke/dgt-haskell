module Main (main) where

import System.Environment (getArgs)
import Lib (createParserState, parseChar, parseString, runParser)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      putStrLn $ "The file name is: " ++ fileName
      putStrLn $ readFile fileName
    _ -> do
      putStrLn "Usage: dtg <filename>"

  let state = createParserState "hellow"
  let result = runParser (parseString "hello") state

  print result
