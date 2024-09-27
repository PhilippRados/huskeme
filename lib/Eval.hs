{-# LANGUAGE OverloadedStrings #-}

module Eval (evalExpr) where

import Data.Text (Text)
import Parser

-- newtype EvalResult = Either EvalError LispVal

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum expr = error $ show expr ++ " not a number"

unpackString :: LispVal -> Text
unpackString (String s) = s
unpackString expr = error $ show expr ++ " not a string"

binOp :: (b -> LispVal) -> (LispVal -> a) -> ([a] -> b) -> [LispVal] -> LispVal
binOp _ _ _ [_] = error "expected at least two arguments, got 1"
binOp _ _ _ [] = error "expected at least two arguments, got 0"
binOp pack unpack foldOp args = pack $ foldOp $ map unpack args

arithBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
arithBinOp op = binOp Number unpackNum $ foldl1 op

-- allows folding values to boolean types instead of lists type only
foldComp :: (a -> a -> Bool) -> [a] -> Bool
foldComp _ [] = error "unreachable because binOp checks for enough args"
foldComp op (x' : xs') = innerFold True x' xs'
  where
    innerFold acc _ [] = acc
    innerFold acc prev (x : xs) = innerFold (op prev x && acc) x xs

numCompBinOp :: (Integer -> Integer -> Bool) -> [LispVal] -> LispVal
numCompBinOp op = binOp Bool unpackNum $ foldComp op

strCompBinOp :: (Text -> Text -> Bool) -> [LispVal] -> LispVal
strCompBinOp op = binOp Bool unpackString $ foldComp op

-- NOTE: these are builtin functions according to the r7rs standard:
-- https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html#TAG:__tex2page_chap_6
procedures :: [(Text, [LispVal] -> LispVal)]
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
    ("string>=?", strCompBinOp (>=))
  ]

apply :: Text -> [LispVal] -> LispVal
apply op args = case lookup op procedures of
  Just f -> f args
  Nothing -> error "todo: lookup functions from env"

ifExpr :: LispVal -> LispVal -> LispVal -> LispVal
ifExpr cond then_expr else_expr =
  case evalExpr cond of
    Bool True -> evalExpr then_expr
    _ -> evalExpr else_expr

evalExpr :: LispVal -> LispVal
evalExpr (List [Atom "quote", x]) = x
evalExpr (List [Atom "if", cond, then_expr, else_expr]) = ifExpr cond then_expr else_expr
evalExpr (List [Atom "if", cond, then_expr]) = ifExpr cond then_expr Undefined
evalExpr (List (Atom op : rest)) = apply op $ map evalExpr rest
evalExpr (List (op : _)) = error $ show op ++ " is not a function, must be atom"
-- evalExpr (DottedList (op : rest)) = apply op $ map evalExpr rest
evalExpr (Atom a) = error "lookup a value"
evalExpr expr = expr
