{-# LANGUAGE OverloadedStrings #-}

module Eval (eval, EvalResult) where

import Builtins
import Control.Monad.Except
import Control.Monad.State
import Data.List (findIndex)
import qualified Data.Map as Map
import qualified Data.Text as T
import Error
import Types

applyOp :: LispVal -> [LispVal] -> EvalResult LispVal
applyOp first rest = do
  op <- evalExpr first
  args <- mapM evalExpr rest
  case op of
    Func (InternalFn f) -> f args
    (Lambda params body) ->
      if length params /= length args
        then throwError $ ArgError (length params) (length args)
        else do
          enter
          _ <- zipWithM define params args
          result <- evalBody
          exit
          return $ last result
      where
        evalBody = mapM evalExpr body
    _ -> throwError $ BasicError "operator must be a function"

ifExpr :: LispVal -> LispVal -> LispVal -> EvalResult LispVal
ifExpr cond then_expr else_expr = do
  cond' <- evalExpr cond
  case cond' of
    -- Of all the Scheme values, only #f counts as false in conditional expressions. All other Scheme values, including #t, count as true.
    Bool False -> evalExpr else_expr
    _ -> evalExpr then_expr

define :: T.Text -> LispVal -> EvalResult LispVal
define name value = do
  modify addToLastEnv
  return Undefined
  where
    addToLastEnv (current : rest) =
      let new_current = Map.insert name value current
       in new_current : rest
    addToLastEnv [] = error "unreachable: global environment should always exist"

getVar :: T.Text -> EvalResult LispVal
getVar ident = do
  env <- get
  searchEnv env
  where
    searchEnv :: [Env] -> EvalResult LispVal
    searchEnv (current : rest) = case Map.lookup ident current of
      Just n -> return n
      Nothing ->
        if null rest
          then throwError $ UnboundVar ident
          else searchEnv rest
    searchEnv [] = error "unreachable: global environment should always exist"

setVar :: [LispVal] -> EvalResult LispVal
setVar [Atom ident, expr] = do
  val <- evalExpr expr
  env <- get
  pos <- case findIndex (Map.member ident) env of
    Just n -> return n
    Nothing -> throwError $ UnboundVar ident
  modify $ updateEnvAtPos val pos
  return Undefined
  where
    -- NOTE: this pattern match does not fail because if pos is 0 then snd contains elems and cannot be empty
    updateEnvAtPos val pos env = let (x, xs : ys) = splitAt pos env in x ++ Map.insert ident val xs : ys
setVar args = throwError $ ArgError 2 (length args)

enter :: EvalResult ()
enter =
  modify ((Map.empty :: Map.Map T.Text LispVal) :)

exit :: EvalResult ()
exit =
  modify tail

lambda :: [LispVal] -> [LispVal] -> EvalResult LispVal
lambda args body = mapM unpackAtom args >>= \params -> return (Lambda params body)
  where
    unpackAtom :: LispVal -> EvalResult T.Text
    unpackAtom (Atom ident) = return ident
    unpackAtom _ = throwError $ TypeError "identifier"

evalExpr :: LispVal -> EvalResult LispVal
evalExpr (List [Atom "quote", expr]) = return expr
evalExpr (List [Atom "if", cond, then_expr, else_expr]) = ifExpr cond then_expr else_expr
evalExpr (List [Atom "if", cond, then_expr]) = ifExpr cond then_expr Undefined
evalExpr (List (Atom "if" : args)) = throwError $ ArgError 2 (length args)
evalExpr (List [Atom "define", Atom name, expr]) = define name expr
evalExpr (List (Atom "define" : List (Atom name : args) : body)) = lambda args body >>= define name
evalExpr (List (Atom "define" : _)) = throwError $ BasicError "define expects identifier or function definition"
evalExpr (List (Atom "lambda" : List args : body)) = lambda args body
evalExpr (List (Atom "set!" : args)) = setVar args
evalExpr (Atom ident) = getVar ident
evalExpr (List (first : rest)) = applyOp first rest
evalExpr (DottedList _ _) = throwError $ BasicError "cannot evaluate improper list"
evalExpr expr = return expr

eval :: [LispVal] -> Either SchemeError LispVal
eval exprs = case runExcept $ evalStateT (mapM evalExpr exprs) builtinEnv of
  Left err -> Left $ Eval err
  Right val -> return $ last val
