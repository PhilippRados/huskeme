{-# LANGUAGE OverloadedStrings #-}

module Eval (applyOp, runWithEnv, evalExpr, EvalResult) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace
import Parser
import Utils

applyOp :: LispVal -> [LispVal] -> Loc -> EvalResult LispVal
applyOp op args loc = do
  case op of
    Func (InternalFn f) -> f args loc
    (Lambda params varargs closure body) ->
      if mismatchedArgs params args varargs
        then throwError $ ArgError (length params) (length args) loc
        else evalLambda params args varargs closure body loc
    op' -> throwError $ TypeError "function" op' loc

mismatchedArgs :: [T.Text] -> [LispVal] -> Maybe T.Text -> Bool
mismatchedArgs params args varargs =
  length params /= length args && isNothing varargs
    || length params > length args && isJust varargs

evalLambda :: [T.Text] -> [LispVal] -> Maybe T.Text -> EnvRefs -> [LispVal] -> Loc -> EvalResult LispVal
evalLambda params args varargs closureRefs body loc = do
  old_env <- get
  enter
  zipWithM_ addToEnv params args
  _ <- maybeDefVarargs varargs
  result <- evalBody
  exit (envRefs old_env)
  return $ last result
  where
    evalBody = mapM evalExpr body

    remainingArgs = drop (length params) args
    maybeDefVarargs (Just varargs') = addToEnv varargs' (List remainingArgs loc)
    maybeDefVarargs Nothing = return Undefined

    enter = modify (\env -> env {envRefs = Map.union closureRefs (envRefs env)})
    exit :: EnvRefs -> EvalResult ()
    exit oldRefs = modify (\env -> env {envRefs = oldRefs})

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
  env <- get
  case Map.lookup name $ envRefs env of
    Just ref -> modify (updateRef ref value) >> return Undefined
    Nothing -> addToEnv name value

addToEnv :: T.Text -> LispVal -> EvalResult LispVal
addToEnv name value = do
  modify go
  return Undefined
  where
    go Env {envRefs = refs, envVals = vals} =
      let ref = (+ 1) $ maximum (Map.keys vals)
          new_vals = Map.insert ref value vals
          new_refs = Map.insert name ref refs
       in Env {envRefs = new_refs, envVals = new_vals}

getVar :: T.Text -> Loc -> EvalResult LispVal
getVar ident loc = do
  Env {envRefs = refs, envVals = vals} <- get
  case Map.lookup ident refs of
    Just n -> return $ fromJust (Map.lookup n vals)
    Nothing -> throwError $ UnboundVar ident loc

setVar :: [LispVal] -> Loc -> EvalResult LispVal
setVar [Atom ident loc, expr] _ = do
  value <- evalExpr expr
  Env {envRefs = refs} <- get
  case Map.lookup ident refs of
    Just ref -> do
      modify $ updateRef ref value
      return Undefined
    Nothing -> throwError $ UnboundVar ident loc
setVar args loc = throwError $ ArgError 2 (length args) loc

updateRef :: Int -> LispVal -> Env -> Env
updateRef ref value env@Env {envVals = vals} = env {envVals = Map.insert ref value vals}

lambda :: [LispVal] -> Maybe LispVal -> [LispVal] -> Loc -> EvalResult LispVal
lambda args varargs body loc = do
  params <- mapM unpackAtom args
  varargs' <- mapM unpackAtom varargs
  Env {envRefs = refs} <- get
  return (Lambda params varargs' refs body)
  where
    unpackAtom :: LispVal -> EvalResult T.Text
    unpackAtom (Atom ident _) = return ident
    unpackAtom arg = throwError $ TypeError "identifier" arg loc

evalExpr :: LispVal -> EvalResult LispVal
evalExpr (List [Atom "quote" _, expr] _) = return expr
evalExpr (List [Atom "if" _, cond, then_expr, else_expr] _) = ifExpr cond then_expr else_expr
evalExpr (List [Atom "if" _, cond, then_expr] _) = ifExpr cond then_expr Undefined
evalExpr (List (Atom "if" _ : args) loc) = throwError $ ArgError 2 (length args) loc
evalExpr (List [Atom "define" _, Atom name _, expr] _) = define name expr
evalExpr (List (Atom "define" _ : Atom _ _ : args) loc) = throwError $ ArgError 2 (length args + 1) loc
evalExpr (List (Atom "define" _ : List (Atom name _ : args) _ : body) loc) = lambda args Nothing body loc >>= define name
evalExpr (List (Atom "define" _ : DottedList (Atom name _ : args) varargs _ : body) loc) = lambda args (Just varargs) body loc >>= define name
evalExpr (List (Atom "define" _ : arg : _) loc) = throwError $ TypeError "identifier or function definition" arg loc
evalExpr (List (Atom "lambda" _ : (List args _) : body) loc) = lambda args Nothing body loc
evalExpr (List (Atom "lambda" _ : (DottedList args varargs _) : body) loc) = lambda args (Just varargs) body loc
evalExpr (List (Atom "lambda" _ : varargs@(Atom _ _) : body) loc) = lambda [] (Just varargs) body loc
evalExpr (List (Atom "set!" _ : args) loc) = setVar args loc
evalExpr (Atom ident loc) = getVar ident loc
evalExpr (List (first : rest) loc) = do
  op <- evalExpr first
  args <- mapM evalExpr rest
  applyOp op args loc
evalExpr x@(DottedList _ _ loc) = throwError $ TypeError "proper list" x loc
evalExpr x@(List _ loc) = throwError $ TypeError "non-empty list" x loc
evalExpr expr = return expr

evalWithEnv :: [LispVal] -> EvalResult LispVal
evalWithEnv exprs = mapM evalExpr exprs >>= return . last

runWithEnv :: String -> String -> EvalResult LispVal
runWithEnv input filename = lift (except $ readExprs input filename) >>= evalWithEnv
