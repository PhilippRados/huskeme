module Lib (runScheme, runRepl) where

import Error
import Eval
import Repl

runScheme :: String -> IO ()
runScheme input = do
  result <- run input
  case result of
    Left err -> print $ formatError err input
    Right _ -> return ()
