module Main where

import Data.Text as T
import Error
import Parser
import System.Environment

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn $ case readExpr $ T.pack expr of
    Left err -> formatParseError err expr
    Right val -> show val
