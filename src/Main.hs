module Main where

import Control.Monad
import Repl
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> void runRepl
    [file] -> readFile file >>= runScheme >>= putStrLn
    _ -> putStrLn "usage: lispeln [<file>]"
