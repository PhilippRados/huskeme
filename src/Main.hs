module Main where

import Control.Monad
import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let useJit = elem "--jit" args
  case args of
    [] -> void runRepl
    [file] -> readFile file >>= \input -> runScheme useJit input file
    _ -> putStrLn "usage: huskeme [--jit] <file>"
