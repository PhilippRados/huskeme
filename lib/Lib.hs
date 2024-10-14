module Lib (runScheme, runRepl, run) where

import Builtins
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Except
import Eval
import Parser
import Repl
import Utils

eval :: [LispVal] -> ExceptT SchemeError IO LispVal
eval exprs = evalStateT (mapM evalExpr exprs) builtinEnv >>= return . last

run :: String -> String -> IO (Either SchemeError LispVal)
run input filename = runExceptT (except (readExprs input filename) >>= eval)

runScheme :: String -> String -> IO ()
runScheme input filename = do
  result <- run input filename
  case result of
    Left err -> printError err
    Right _ -> return ()
