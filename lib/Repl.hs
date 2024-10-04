module Repl (runRepl, runScheme) where

import Control.Monad.Except
import qualified Data.Text as T (pack)
import Error
import Eval
import Parser
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
    Just input -> liftIO (putStrLn $ runScheme input) >> repl

runScheme :: String -> String
runScheme input = case readExprs (T.pack input) >>= eval of
  Left err -> formatError err input
  Right val -> show val
