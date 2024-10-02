{-# LANGUAGE OverloadedStrings #-}

module Builtins (builtinEnv) where

import Control.Monad.Except
import Data.List (find)
import qualified Data.Map as Map
import qualified Data.Text as T
import Error
import Types

unpackNum :: LispVal -> EvalResult Integer
unpackNum (Number n) = return n
unpackNum _ = throwError $ TypeError "number"

unpackString :: LispVal -> EvalResult T.Text
unpackString (String s) = return s
unpackString _ = throwError $ TypeError "string"

binOp :: (b -> LispVal) -> (LispVal -> EvalResult a) -> ([a] -> b) -> [LispVal] -> EvalResult LispVal
binOp _ _ _ [_] = throwError $ ArgError 2 1
binOp _ _ _ [] = throwError $ ArgError 2 0
binOp pack unpack foldOp args = fmap (pack . foldOp) (mapM unpack args)

arithOp :: (Integer -> Integer -> Integer) -> [LispVal] -> EvalResult LispVal
arithOp op = binOp Number unpackNum $ foldl1 op

-- allows folding values to boolean types instead of only the lists type
foldComp :: (a -> a -> Bool) -> [a] -> Bool
foldComp _ [] = error "unreachable because binOp checks for enough args"
foldComp op (x' : xs') = innerFold True x' xs'
  where
    innerFold acc _ [] = acc
    innerFold acc prev (x : xs) = innerFold (op prev x && acc) x xs

numCompOp :: (Integer -> Integer -> Bool) -> [LispVal] -> EvalResult LispVal
numCompOp op = binOp Bool unpackNum $ foldComp op

strCompOp :: (T.Text -> T.Text -> Bool) -> [LispVal] -> EvalResult LispVal
strCompOp op = binOp Bool unpackString $ foldComp op

car :: [LispVal] -> EvalResult LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [_] = throwError $ TypeError "list"
car args = throwError $ ArgError 1 (length args)

cdr :: [LispVal] -> EvalResult LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [_] = throwError $ TypeError "list"
cdr args = throwError $ ArgError 1 (length args)

cons :: [LispVal] -> EvalResult LispVal
cons [x1, List []] = return $ List [x1]
cons [x1, List xs] = return $ List (x1 : xs)
cons [x1, DottedList xs last_] = return $ DottedList (x1 : xs) last_
cons [x1, x2] = return $ DottedList [x1] x2
cons args = throwError $ ArgError 2 (length args)

-- eq and eqv can be the same according to r7rs:
-- It must always return #f when eqv? also would,
-- but may return #f in some cases where eqv? would return #t
eqv :: [LispVal] -> EvalResult LispVal
eqv [Number a, Number b] = return $ Bool $ a == b
eqv [Bool a, Bool b] = return $ Bool $ a == b
eqv [Atom a, Atom b] = return $ Bool $ a == b -- can only happen if symbol because lookup extracts stored value of variable
eqv [String a, String b] = return $ Bool $ a == b
eqv [List [], List []] = return $ Bool True
eqv [_, _] = return $ Bool False
eqv args = throwError $ ArgError 2 (length args)

-- same as eqv but can be used for lists as well
equal :: [LispVal] -> EvalResult LispVal
equal [List xs, List ys] = equalLists xs ys
equal [DottedList xs x, DottedList ys y] = equalLists (xs ++ [x]) (ys ++ [y])
equal args = eqv args

equalLists :: [LispVal] -> [LispVal] -> EvalResult LispVal
equalLists xs ys = equal_values >>= (\eq -> return $ Bool $ same_length && eq)
  where
    same_length = length xs == length ys
    equal_values = do
      values <- mapM (\(a, b) -> equal [a, b]) $ zip xs ys
      return $ all (== Bool True) values

andOp :: [LispVal] -> EvalResult LispVal
andOp [] = return $ Bool True
andOp args = return $ case find (== Bool False) args of
  Just falsy -> falsy
  Nothing -> last args

orOp :: [LispVal] -> EvalResult LispVal
orOp [] = return $ Bool False
orOp args = return $ case find (/= Bool False) args of
  Just truthy -> truthy
  Nothing -> last args

builtinEnv :: [Map.Map T.Text LispVal]
builtinEnv = [Map.fromList $ map toFunc builtins]
  where
    toFunc (ident, f) = (ident, Func (Fn f))

-- these are builtin functions according to the r7rs standard:
-- https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html#TAG:__tex2page_chap_6
builtins :: [(T.Text, [LispVal] -> EvalResult LispVal)]
builtins =
  [ ("+", arithOp (+)),
    ("-", arithOp (-)),
    ("*", arithOp (*)),
    ("/", arithOp div),
    ("mod", arithOp mod),
    ("quotient", arithOp quot),
    ("remainder", arithOp rem),
    ("=", numCompOp (==)),
    ("<", numCompOp (<)),
    (">", numCompOp (>)),
    (">=", numCompOp (>=)),
    ("<=", numCompOp (<=)),
    ("string=?", strCompOp (==)),
    ("string<?", strCompOp (<)),
    ("string>?", strCompOp (>)),
    ("string<=?", strCompOp (<=)),
    ("string>=?", strCompOp (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    -- equality operations in scheme: https://stackoverflow.com/questions/16299246/what-is-the-difference-between-eq-eqv-equal-and-in-scheme
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal),
    ("and", andOp),
    ("or", orOp)
  ]
