module Repl (runRepl) where

import Builtins (builtinEnv)
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
import Types (Env)

runRepl :: IO ()
runRepl = runInputT defaultSettings $ repl builtinEnv

repl :: [Env] -> InputT IO ()
repl env = do
  minput <- getInputLine "Repl> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just ":quit" -> outputStrLn "Goodbye."
    Just input -> do
      (val, env') <- liftIO $ runLine input env
      liftIO $ putStrLn val
      return repl val env'

-- liftIO ( >>= putStrLn) >> repl env

runLine :: String -> [Env] -> IO (String, [Env])
runLine input env = do
  result <- runWithEnv input env
  return $ case result of
    Left err -> (formatError err input, env)
    Right (val, env') -> (show val, env')
