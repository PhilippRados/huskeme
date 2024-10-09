module Repl (runRepl) where

import Control.Monad.Except
import Error
import Eval
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )

runRepl :: IO ()
runRepl = runInputT defaultSettings repl

repl :: InputT IO ()
repl = do
  minput <- getInputLine "Repl> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just ":quit" -> outputStrLn "Goodbye."
    Just input -> liftIO (runLine input >>= putStrLn) >> repl

runLine :: String -> IO String
runLine input = do
  result <- run input
  return $ case result of
    Left err -> formatError err input
    Right val -> show val
