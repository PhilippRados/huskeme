module Lib (runScheme, runRepl) where

import Error
import Eval
import Repl

runScheme :: String -> String -> IO ()
runScheme input filename = do
  result <- run input filename
  case result of
    Left err -> formatError err input filename
    Right _ -> return ()
