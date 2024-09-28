module Main where

import Data.Text as T
import Error (formatError)
import Eval
import Parser
import System.Environment

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn $ case readExpr (T.pack expr) >>= eval of
    Left err -> formatError err expr
    Right val -> show val
