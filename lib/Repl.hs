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

-- TODO: have environment across repl-lines
-- type ReplState a = InputT (StateT [Env] IO) a
-- runRepl :: IO ()
-- runRepl = evalStateT (runInputT defaultSettings repl) builtinEnv
-- repl = do
--   minput <- getInputLine "Repl> "
--   case minput of
--     Nothing -> outputStrLn "Goodbye."
--     Just ":quit" -> outputStrLn "Goodbye."
--     Just input -> runLine input >> repl
-- runLine :: String -> ReplState ()
-- runLine line = do
--   env <- lift get
--   outputStrLn $
--     case readExpr (T.pack line) >>= \expr -> runExcept $ evalStateT (eval expr) env of
--       Left err -> formatError err line
--       Right val -> show val
