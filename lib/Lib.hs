module Lib (runScheme, runRepl) where

import Eval
import Repl
import Utils

runScheme :: String -> String -> IO ()
runScheme input filename = do
  result <- run input filename
  case result of
    Left err -> printError err
    Right _ -> return ()
