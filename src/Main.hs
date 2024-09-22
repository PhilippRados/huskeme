module Main where

import Data.Text as T
import Error
import Eval
import Parser
import System.Environment

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn $ case evalExpr <$> (readExpr $ T.pack expr) of
    Left err -> formatParseError err expr
    Right val -> show val
