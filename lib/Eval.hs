{-# LANGUAGE OverloadedStrings #-}

module Eval (eval) where

import qualified Data.Text as T
import Error
import Parser

type EvalResult a = Either EvalError a

unpackNum :: LispVal -> EvalResult Integer
unpackNum (Number n) = return n
unpackNum _ = Left $ TypeError "number"

unpackString :: LispVal -> EvalResult T.Text
unpackString (String s) = return s
unpackString _ = Left $ TypeError "string"

binOp :: (b -> LispVal) -> (LispVal -> EvalResult a) -> ([a] -> b) -> [LispVal] -> EvalResult LispVal
binOp _ _ _ [_] = Left $ ArgError 2 1
binOp _ _ _ [] = Left $ ArgError 2 0
binOp pack unpack foldOp args = fmap (pack . foldOp) (mapM unpack args)

arithBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> EvalResult LispVal
arithBinOp op = binOp Number unpackNum $ foldl1 op

-- allows folding values to boolean types instead of only the lists type
foldComp :: (a -> a -> Bool) -> [a] -> Bool
foldComp _ [] = error "unreachable because binOp checks for enough args"
foldComp op (x' : xs') = innerFold True x' xs'
  where
    innerFold acc _ [] = acc
    innerFold acc prev (x : xs) = innerFold (op prev x && acc) x xs

numCompBinOp :: (Integer -> Integer -> Bool) -> [LispVal] -> EvalResult LispVal
numCompBinOp op = binOp Bool unpackNum $ foldComp op

strCompBinOp :: (T.Text -> T.Text -> Bool) -> [LispVal] -> EvalResult LispVal
strCompBinOp op = binOp Bool unpackString $ foldComp op

car :: [LispVal] -> EvalResult LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [_] = Left $ TypeError "list"
car args = Left $ ArgError 1 (length args)

cdr :: [LispVal] -> EvalResult LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [_] = Left $ TypeError "list"
cdr args = Left $ ArgError 1 (length args)

cons :: [LispVal] -> EvalResult LispVal
cons [x1, List []] = return $ List [x1]
cons [x1, List xs] = return $ List (x1 : xs)
cons [x1, DottedList xs last_] = return $ DottedList (x1 : xs) last_
cons [x1, x2] = return $ DottedList [x1] x2
cons args = Left $ ArgError 2 (length args)

-- NOTE: these are builtin functions according to the r7rs standard:
-- https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html#TAG:__tex2page_chap_6
procedures :: [(T.Text, [LispVal] -> EvalResult LispVal)]
procedures =
  [ ("+", arithBinOp (+)),
    ("-", arithBinOp (-)),
    ("*", arithBinOp (*)),
    ("/", arithBinOp div),
    ("mod", arithBinOp mod),
    ("quotient", arithBinOp quot),
    ("remainder", arithBinOp rem),
    ("=", numCompBinOp (==)),
    ("<", numCompBinOp (<)),
    (">", numCompBinOp (>)),
    (">=", numCompBinOp (>=)),
    ("<=", numCompBinOp (<=)),
    -- ("and", logicBinOp (&&)),
    -- ("or", logicBinOp (||)),
    ("string=?", strCompBinOp (==)),
    ("string<?", strCompBinOp (<)),
    ("string>?", strCompBinOp (>)),
    ("string<=?", strCompBinOp (<=)),
    ("string>=?", strCompBinOp (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons)
  ]

apply :: T.Text -> [LispVal] -> EvalResult LispVal
apply op args = case lookup op procedures of
  Just f -> f args
  Nothing -> Left $ BasicError $ T.concat [op, " function does not exist"]

ifExpr :: LispVal -> LispVal -> LispVal -> EvalResult LispVal
ifExpr cond then_expr else_expr = do
  cond' <- evalExpr cond
  case cond' of
    Bool True -> evalExpr then_expr
    _ -> evalExpr else_expr

evalExpr :: LispVal -> EvalResult LispVal
evalExpr (List [Atom "quote", x]) = return x
evalExpr (List [Atom "if", cond, then_expr, else_expr]) = ifExpr cond then_expr else_expr
evalExpr (List [Atom "if", cond, then_expr]) = ifExpr cond then_expr Undefined
evalExpr (List (Atom op : rest)) = mapM evalExpr rest >>= apply op
evalExpr (List (op : _)) = Left $ BasicError "operator must be a function"
-- evalExpr (DottedList (op : rest)) = apply op $ map evalExpr rest
evalExpr (Atom _) = error "todo: lookup a value"
evalExpr expr = return expr

eval :: LispVal -> Either SchemeError LispVal
eval expr = case evalExpr expr of
  Left err -> Left $ Eval err
  Right val -> return val
