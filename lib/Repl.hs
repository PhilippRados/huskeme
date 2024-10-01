module Repl (runRepl, runLine) where

import Control.Monad.Trans (MonadIO (liftIO))
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
    Just input -> liftIO (putStrLn $ runLine input) >> repl

runLine :: String -> String
runLine input = case readExpr (T.pack input) >>= eval of
  Left err -> formatError err input
  Right val -> show val
