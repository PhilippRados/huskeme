{-# LANGUAGE OverloadedStrings #-}

module Eval (run, runWithEnv, EvalResult) where

import Builtins
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.List (findIndex)
import qualified Data.Map as Map
import qualified Data.Text as T
import Parser
import Text.Parsec (SourcePos)
import Utils

applyOp :: LispVal -> [LispVal] -> SourcePos -> EvalResult LispVal
applyOp first rest pos = do
  op <- evalExpr first
  args <- mapM evalExpr rest
  case op of
    Func (InternalFn f) -> f args pos
    (Lambda params body) ->
      if length params /= length args
        then throwError $ ArgError (length params) (length args) pos
        else do
          enter
          zipWithM_ define params args
          result <- evalBody
          exit
          return $ last result
      where
        evalBody = mapM evalExpr body
    op -> throwError $ TypeError "function" op pos

ifExpr :: LispVal -> LispVal -> LispVal -> EvalResult LispVal
ifExpr cond then_expr else_expr = do
  cond' <- evalExpr cond
  case cond' of
    -- Of all the Scheme values, only #f counts as false in conditional expressions. All other Scheme values, including #t, count as true.
    Bool False -> evalExpr else_expr
    _ -> evalExpr then_expr

define :: T.Text -> LispVal -> EvalResult LispVal
define name expr = do
  value <- evalExpr expr
  modify (addToLastEnv value)
  return Undefined
  where
    addToLastEnv value (current : rest) =
      let new_current = Map.insert name value current
       in new_current : rest
    addToLastEnv _ [] = error "unreachable: global environment should always exist"

getVar :: T.Text -> SourcePos -> EvalResult LispVal
getVar ident pos = do
  env <- get
  searchEnv env
  where
    searchEnv :: [Env] -> EvalResult LispVal
    searchEnv (current : rest) = case Map.lookup ident current of
      Just n -> return n
      Nothing ->
        if null rest
          then throwError $ UnboundVar ident pos
          else searchEnv rest
    searchEnv [] = error "unreachable: global environment should always exist"

setVar :: [LispVal] -> SourcePos -> EvalResult LispVal
setVar [Atom ident errPos, expr] _ = do
  val <- evalExpr expr
  env <- get
  pos <- case findIndex (Map.member ident) env of
    Just n -> return n
    Nothing -> throwError $ UnboundVar ident errPos
  modify $ updateEnvAtPos val pos
  return Undefined
  where
    -- NOTE: this pattern match does not fail because if pos is 0 then snd contains elems and cannot be empty
    updateEnvAtPos val pos env = let (x, xs : ys) = splitAt pos env in x ++ Map.insert ident val xs : ys
setVar args pos = throwError $ ArgError 2 (length args) pos

enter :: EvalResult ()
enter =
  modify ((Map.empty :: Map.Map T.Text LispVal) :)

exit :: EvalResult ()
exit =
  modify tail

lambda :: [LispVal] -> [LispVal] -> SourcePos -> EvalResult LispVal
lambda args body pos = mapM unpackAtom args >>= \params -> return (Lambda params body)
  where
    unpackAtom :: LispVal -> EvalResult T.Text
    unpackAtom (Atom ident _) = return ident
    unpackAtom arg = throwError $ TypeError "identifier" arg pos

evalExpr :: LispVal -> EvalResult LispVal
evalExpr (List [Atom "quote" _, expr] _) = return expr
evalExpr (List [Atom "if" _, cond, then_expr, else_expr] _) = ifExpr cond then_expr else_expr
evalExpr (List [Atom "if" _, cond, then_expr] _) = ifExpr cond then_expr Undefined
evalExpr (List (Atom "if" _ : args) pos) = throwError $ ArgError 2 (length args) pos
evalExpr (List [Atom "define" _, Atom name _, expr] _) = define name expr
evalExpr (List (Atom "define" _ : Atom _ _ : args) pos) = throwError $ ArgError 2 (length args + 1) pos
evalExpr (List (Atom "define" _ : List (Atom name _ : args) _ : body) pos) = lambda args body pos >>= define name
evalExpr (List (Atom "define" _ : arg : _) pos) = throwError $ TypeError "identifier or function definition" arg pos
evalExpr (List (Atom "lambda" _ : (List args _) : body) pos) = lambda args body pos
evalExpr (List (Atom "set!" _ : args) pos) = setVar args pos
evalExpr (Atom ident pos) = getVar ident pos
evalExpr (List (first : rest) pos) = applyOp first rest pos
evalExpr x@(DottedList _ _ pos) = throwError $ TypeError "proper list" x pos
evalExpr x@(List _ pos) = throwError $ TypeError "non-empty list" x pos
evalExpr expr = return expr

evalWithEnv :: [LispVal] -> StateT [Env] (ExceptT SchemeError IO) LispVal
evalWithEnv exprs = do
  env <- get
  (vals, env') <- lift (withExceptT Eval $ runStateT (mapM evalExpr exprs) env)
  modify $ const env'
  return $ last vals

runWithEnv :: String -> String -> StateT [Env] (ExceptT SchemeError IO) LispVal
runWithEnv input filename = do
  exprs <- lift $ except (readExprs input filename)
  evalWithEnv exprs

eval :: [LispVal] -> ExceptT SchemeError IO LispVal
eval exprs = do
  result <- withExceptT Eval $ evalStateT (mapM evalExpr exprs) builtinEnv
  return $ last result

run :: String -> String -> IO (Either SchemeError LispVal)
run input filename = runExceptT (except (readExprs input filename) >>= eval)
