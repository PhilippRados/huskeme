module Lib (runScheme, runRepl, run,eval) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Except
import Interpreter.Builtins
import Interpreter.Eval
import Parser
import Repl
import Utils

type Evaluator a = [LispVal] -> ExceptT SchemeError IO a

eval :: Evaluator LispVal
eval exprs = evalStateT (mapM evalExpr exprs) builtinEnv >>= return . last

emitLLVM :: Evaluator LispVal
emitLLVM exprs = undefined

run :: Evaluator a -> String -> String -> IO (Either SchemeError a)
run evaluator input filename = runExceptT (except (readExprs input filename) >>= evaluator)

runScheme :: Bool -> String -> String -> IO ()
runScheme useJit input filename = do
  let evaluator = if useJit then emitLLVM else eval
  result <- run evaluator input filename
  case result of
    Left err -> printError err
    Right _ -> return ()
