{-# LANGUAGE OverloadedStrings #-}

module Eval (applyOp, runWithEnv, evalExpr, EvalResult) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.List (findIndex)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
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

evalLambda :: [T.Text] -> [LispVal] -> Maybe T.Text -> [Env] -> [LispVal] -> Loc -> EvalResult LispVal
evalLambda params args varargs closure body loc = do
  enter
  zipWithM_ addToLastEnv params args
  _ <- maybeDefVarargs varargs
  result <- evalBody
  exit
  return $ last result
  where
    evalBody = mapM evalExpr body
    remainingArgs = drop (length params) args
    maybeDefVarargs (Just varargs') = addToLastEnv varargs' (List remainingArgs loc)
    maybeDefVarargs Nothing = return Undefined
    enter = modify (closure ++) -- FIXME: closures global env overshadows current global
    -- enter = modify ((Map.empty :: Map.Map T.Text LispVal) :)
    exit = modify (drop $ length closure)

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
  addToLastEnv name value

addToLastEnv :: T.Text -> LispVal -> EvalResult LispVal
addToLastEnv name value = do
  modify go
  return Undefined
  where
    go (current : rest) =
      let new_current = Map.insert name value current
       in new_current : rest
    go [] = error "unreachable: global environment should always exist"

getVar :: T.Text -> Loc -> EvalResult LispVal
getVar ident loc = do
  env <- get
  searchEnv env
  where
    searchEnv :: [Env] -> EvalResult LispVal
    searchEnv (current : rest) = case Map.lookup ident current of
      Just n -> return n
      Nothing ->
        if null rest
          then throwError $ UnboundVar ident loc
          else searchEnv rest
    searchEnv [] = error "unreachable: global environment should always exist"

setVar :: [LispVal] -> Loc -> EvalResult LispVal
setVar [Atom ident loc, expr] _ = do
  val <- evalExpr expr
  env <- get
  pos <- case findIndex (Map.member ident) env of
    Just n -> return n
    Nothing -> throwError $ UnboundVar ident loc
  modify $ updateEnvAtPos val pos
  return Undefined
  where
    -- NOTE: this pattern match does not fail because if pos is 0 then snd contains elems and cannot be empty
    updateEnvAtPos val pos env = let (x, xs : ys) = splitAt pos env in x ++ Map.insert ident val xs : ys
setVar args loc = throwError $ ArgError 2 (length args) loc

lambda :: [LispVal] -> Maybe LispVal -> [LispVal] -> Loc -> EvalResult LispVal
lambda args varargs body loc = do
  params <- mapM unpackAtom args
  varargs' <- mapM unpackAtom varargs
  env <- get
  return (Lambda params varargs' env body)
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
