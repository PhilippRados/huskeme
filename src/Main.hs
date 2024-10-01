module Main where

import qualified Data.Text as T
import Error (formatError)
import Eval
import Parser
import Repl
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl >> return ()
    [file] -> readFile file >>= putStrLn . runLine
    _ -> putStrLn "usage: lispeln [<file>]"
