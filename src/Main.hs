module Main where

import System.Environment (getArgs)

import Parse (parse)
import Eval (eval)

run :: FilePath -> String -> IO ()
run f s = case parse f s of
  Left err -> print err
  Right ast -> eval ast

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> putStrLn "Please specify a file!"
    [p] -> do
      f <- readFile p
      run p f
    _   -> putStrLn "Too many arguments, not sure what to do."