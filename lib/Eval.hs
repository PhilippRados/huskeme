{-# LANGUAGE OverloadedStrings #-}

module Eval (eval, EvalResult) where

import Builtins
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import Error
import Types

applyOp :: LispVal -> [LispVal] -> EvalResult LispVal
applyOp op args = do
  op' <- evalExpr op
  args' <- mapM evalExpr args
  case op' of
    Func (Fn f) -> f args'
    _ -> throwError $ BasicError "operator must be a function"

ifExpr :: LispVal -> LispVal -> LispVal -> EvalResult LispVal
ifExpr cond then_expr else_expr = do
  cond' <- evalExpr cond
  case cond' of
    -- Of all the Scheme values, only #f counts as false in conditional expressions. All other Scheme values, including #t, count as true.
    Bool False -> evalExpr else_expr
    _ -> evalExpr then_expr

defineVar :: LispVal -> LispVal -> EvalResult LispVal
defineVar (Atom ident) expr = do
  modify addToLastEnv
  return Undefined
  where
    addToLastEnv (current : rest) =
      let new_current = Map.insert ident expr current
       in new_current : rest
    addToLastEnv [] = error "unreachable: global environment should always exist"
defineVar _ _ = throwError $ TypeError "identifier"

getVar :: T.Text -> EvalResult LispVal
getVar ident = do
  env <- get
  -- TODO: flatten all envs into single big env to search
  case Map.lookup ident (head env) of
    Just n -> return n
    Nothing -> throwError $ UnboundVar ident

-- TODO: would be better to do with ReaderT and local for automatic scoping
enter :: EvalResult ()
enter =
  modify ((Map.empty :: Map.Map T.Text LispVal) :)

exit :: EvalResult ()
exit =
  modify tail

evalExpr :: LispVal -> EvalResult LispVal
evalExpr (List [Atom "quote", expr]) = return expr
evalExpr (List [Atom "if", cond, then_expr, else_expr]) = ifExpr cond then_expr else_expr
evalExpr (List [Atom "if", cond, then_expr]) = ifExpr cond then_expr Undefined
evalExpr (List [Atom "define", ident, expr]) = defineVar ident expr
evalExpr (Atom ident) = getVar ident
evalExpr (List (first : rest)) = applyOp first rest
evalExpr (DottedList _ _) = throwError $ BasicError "cannot evaluate improper list"
evalExpr expr = return expr

-- TODO: have to change this for repl session
-- eval :: LispVal -> StateT [Env] (Except SchemeError) LispVal
-- eval expr = do
--   env <- get
--   lift $ withExcept Eval $ evalStateT (evalExpr expr) env

eval :: [LispVal] -> Either SchemeError LispVal
eval exprs = case runExcept $ evalStateT (mapM evalExpr exprs) builtinEnv of
  Left err -> Left $ Eval err
  Right val -> return $ last val
