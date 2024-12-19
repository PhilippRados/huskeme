module Jit.Codegen where

data CodegenState = CodegenState
  { -- List of all Functions in module.
    -- Cannot be empty since always have main function
    cgFunctions :: [Function],
    -- Index to current function where instructions are inserted into
    cgCurrentFunc :: Int,
    cgEnv :: Env
  }

type CodegenResult = StateT CodegenState (ExceptT SchemeError)

applyOp :: CodegenResult
applyOp op args loc = do

lowerExpr :: LispVal -> CodegenResult
lowerExpr (List [Atom "quote" _, expr] _) = return expr
-- lowerExpr (List [Atom "if" _, cond, then_expr, else_expr] _) = ifExpr cond then_expr else_expr
-- lowerExpr (List [Atom "if" _, cond, then_expr] _) = ifExpr cond then_expr Undefined
-- lowerExpr (List (Atom "if" _ : args) loc) = throwError $ ArgError 2 (length args) loc
-- lowerExpr (List [Atom "define" _, Atom name _, expr] _) = define name expr
-- lowerExpr (List (Atom "define" _ : Atom _ _ : args) loc) = throwError $ ArgError 2 (length args + 1) loc
-- lowerExpr (List (Atom "define" _ : List (Atom name _ : args) _ : body) loc) = lambda args Nothing body loc >>= define name
-- lowerExpr (List (Atom "define" _ : DottedList (Atom name _ : args) varargs _ : body) loc) = lambda args (Just varargs) body loc >>= define name
-- lowerExpr (List (Atom "define" _ : arg : _) loc) = throwError $ TypeError "identifier or function definition" arg loc
-- lowerExpr (List (Atom "lambda" _ : (List args _) : body) loc) = lambda args Nothing body loc
-- lowerExpr (List (Atom "lambda" _ : (DottedList args varargs _) : body) loc) = lambda args (Just varargs) body loc
-- lowerExpr (List (Atom "lambda" _ : varargs@(Atom _ _) : body) loc) = lambda [] (Just varargs) body loc
-- lowerExpr (List (Atom "set!" _ : args) loc) = setVar args loc
-- lowerExpr (Atom ident loc) = getVar ident loc
lowerExpr (List (first : rest) loc) = do
  -- op <- lowerExpr first
  -- args <- mapM lowerExpr rest
  applyOp first rest loc
lowerExpr x@(DottedList _ _ loc) = throwError $ TypeError "proper list" x loc
lowerExpr x@(List _ loc) = throwError $ TypeError "non-empty list" x loc
lowerExpr expr = return expr
