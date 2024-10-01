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
    [file] -> readFile file >>= putStrLn . runFile
    _ -> putStrLn "usage: lispeln [<file>]"

runFile :: String -> String
runFile input = case readExpr (T.pack input) >>= eval of
  Left err -> formatError err input
  Right val -> show val
