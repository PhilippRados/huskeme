module Main where

import Control.Monad
import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> void runRepl
    [file] -> readFile file >>= \input -> runScheme input file
    _ -> putStrLn "usage: huskeme [<file>]"
