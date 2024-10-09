module Main where

import Control.Monad
import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> void runRepl
    [file] -> readFile file >>= runScheme
    _ -> putStrLn "usage: lispeln [<file>]"
