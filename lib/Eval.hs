{-# LANGUAGE OverloadedStrings #-}

module Eval (run, runWithEnv, EvalResult) where

import Builtins
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.List (findIndex)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
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
    (Lambda params varargs body) ->
      if mismatchedArgs params args varargs
        then throwError $ ArgError (length params) (length args) pos
        else evalLambda params args varargs body pos
    op' -> throwError $ TypeError "function" op' pos

mismatchedArgs :: [T.Text] -> [LispVal] -> Maybe T.Text -> Bool
mismatchedArgs params args varargs =
  length params /= length args && isNothing varargs
    || length params > length args && isJust varargs

evalLambda :: [T.Text] -> [LispVal] -> Maybe T.Text -> [LispVal] -> SourcePos -> EvalResult LispVal
evalLambda params args varargs body pos = do
  enter
  zipWithM_ addToLastEnv params args
  _ <- maybeDefVarargs varargs
  result <- evalBody
  exit
  return $ last result
  where
    evalBody = mapM evalExpr body
    remainingArgs = drop (length params) args
    maybeDefVarargs (Just varargs') = addToLastEnv varargs' (List remainingArgs pos)
    maybeDefVarargs Nothing = return Undefined

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

lambda :: [LispVal] -> Maybe LispVal -> [LispVal] -> SourcePos -> EvalResult LispVal
lambda args varargs body pos = do
  params <- mapM unpackAtom args
  varargs' <- mapM unpackAtom varargs
  return (Lambda params varargs' body)
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
evalExpr (List (Atom "define" _ : List (Atom name _ : args) _ : body) pos) = lambda args Nothing body pos >>= define name
evalExpr (List (Atom "define" _ : DottedList (Atom name _ : args) varargs _ : body) pos) = lambda args (Just varargs) body pos >>= define name
evalExpr (List (Atom "define" _ : arg : _) pos) = throwError $ TypeError "identifier or function definition" arg pos
evalExpr (List (Atom "lambda" _ : (List args _) : body) pos) = lambda args Nothing body pos
evalExpr (List (Atom "lambda" _ : (DottedList args varargs _) : body) pos) = lambda args (Just varargs) body pos
evalExpr (List (Atom "lambda" _ : varargs@(Atom _ _) : body) pos) = lambda [] (Just varargs) body pos
evalExpr (List (Atom "set!" _ : args) pos) = setVar args pos
evalExpr (Atom ident pos) = getVar ident pos
evalExpr (List (first : rest) pos) = applyOp first rest pos
evalExpr x@(DottedList _ _ pos) = throwError $ TypeError "proper list" x pos
evalExpr x@(List _ pos) = throwError $ TypeError "non-empty list" x pos
evalExpr expr = return expr

evalWithEnv :: [LispVal] -> EvalResult LispVal
evalWithEnv exprs = mapM evalExpr exprs >>= return . last

runWithEnv :: String -> String -> EvalResult LispVal
runWithEnv input filename = lift (except $ readExprs input filename) >>= evalWithEnv

eval :: [LispVal] -> ExceptT SchemeError IO LispVal
eval exprs = evalStateT (mapM evalExpr exprs) builtinEnv >>= return . last

run :: String -> String -> IO (Either SchemeError LispVal)
run input filename = runExceptT (except (readExprs input filename) >>= eval)
