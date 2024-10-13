{-# LANGUAGE OverloadedStrings #-}

module Builtins (builtinEnv) where

import Control.Monad.Except
import Data.List (find)
import qualified Data.Map as Map
import qualified Data.Text as T
import System.IO
import Text.Parsec (SourcePos)
import Utils

unpackNum :: LispVal -> SourcePos -> EvalResult Integer
unpackNum (Number n) _ = return n
unpackNum x pos = throwError $ TypeError "number" x pos

unpackString :: LispVal -> SourcePos -> EvalResult T.Text
unpackString (String s) _ = return s
unpackString x pos = throwError $ TypeError "string" x pos

binOp :: (b -> LispVal) -> (LispVal -> SourcePos -> EvalResult a) -> ([a] -> b) -> [LispVal] -> SourcePos -> EvalResult LispVal
binOp _ _ _ [_] pos = throwError $ ArgError 2 1 pos
binOp _ _ _ [] pos = throwError $ ArgError 2 0 pos
binOp pack unpack foldOp args pos = fmap (pack . foldOp) (mapM (`unpack` pos) args)

arithOp :: (Integer -> Integer -> Integer) -> [LispVal] -> SourcePos -> EvalResult LispVal
arithOp op = binOp Number unpackNum (foldl1 op)

-- allows folding values to boolean types instead of only the lists type
foldComp :: (a -> a -> Bool) -> [a] -> Bool
foldComp _ [] = error "unreachable because binOp checks for enough args"
foldComp op (x' : xs') = innerFold True x' xs'
  where
    innerFold acc _ [] = acc
    innerFold acc prev (x : xs) = innerFold (op prev x && acc) x xs

numCompOp :: (Integer -> Integer -> Bool) -> [LispVal] -> SourcePos -> EvalResult LispVal
numCompOp op = binOp Bool unpackNum $ foldComp op

strCompOp :: (T.Text -> T.Text -> Bool) -> [LispVal] -> SourcePos -> EvalResult LispVal
strCompOp op = binOp Bool unpackString $ foldComp op

car :: [LispVal] -> SourcePos -> EvalResult LispVal
car [List (x : _) _] _ = return x
car [DottedList (x : _) _ _] _ = return x
car [arg] pos = throwError $ TypeError "list" arg pos
car args pos = throwError $ ArgError 1 (length args) pos

cdr :: [LispVal] -> SourcePos -> EvalResult LispVal
cdr [List (_ : xs) _] pos = return $ List xs pos
cdr [DottedList [_] x _] _ = return x
cdr [DottedList (_ : xs) x _] pos = return $ DottedList xs x pos
cdr [arg] pos = throwError $ TypeError "list" arg pos
cdr args pos = throwError $ ArgError 1 (length args) pos

cons :: [LispVal] -> SourcePos -> EvalResult LispVal
cons [x1, List [] _] pos = return $ List [x1] pos
cons [x1, List xs _] pos = return $ List (x1 : xs) pos
cons [x1, DottedList xs last_ _] pos = return $ DottedList (x1 : xs) last_ pos
cons [x1, x2] pos = return $ DottedList [x1] x2 pos
cons args pos = throwError $ ArgError 2 (length args) pos

-- eq and eqv can be the same according to r7rs:
-- It must always return #f when eqv? also would,
-- but may return #f in some cases where eqv? would return #t
eqv :: [LispVal] -> SourcePos -> EvalResult LispVal
eqv [Number a, Number b] _ = return $ Bool $ a == b
eqv [Bool a, Bool b] _ = return $ Bool $ a == b
eqv [Atom a _, Atom b _] _ = return $ Bool $ a == b -- can only happen if symbol because lookup extracts stored value of variable
eqv [String a, String b] _ = return $ Bool $ a == b
eqv [List [] _, List [] _] _ = return $ Bool True
eqv [_, _] _ = return $ Bool False
eqv args pos = throwError $ ArgError 2 (length args) pos

-- same as eqv but can be used for lists as well
equal :: [LispVal] -> SourcePos -> EvalResult LispVal
equal [List xs _, List ys _] pos = equalLists xs ys pos
equal [DottedList xs x _, DottedList ys y _] pos = equalLists (xs ++ [x]) (ys ++ [y]) pos
equal args pos = eqv args pos

equalLists :: [LispVal] -> [LispVal] -> SourcePos -> EvalResult LispVal
equalLists xs ys pos = equal_values >>= (\eq -> return $ Bool $ same_length && eq)
  where
    same_length = length xs == length ys
    equal_values = do
      values <- mapM (\(a, b) -> equal [a, b] pos) $ zip xs ys
      return $ all (== Bool True) values

andOp :: [LispVal] -> SourcePos -> EvalResult LispVal
andOp [] _ = return $ Bool True
andOp args _ = return $ case find (== Bool False) args of
  Just falsy -> falsy
  Nothing -> last args

orOp :: [LispVal] -> SourcePos -> EvalResult LispVal
orOp [] _ = return $ Bool False
orOp args _ = return $ case find (/= Bool False) args of
  Just truthy -> truthy
  Nothing -> last args

makePort :: IOMode -> [LispVal] -> SourcePos -> EvalResult LispVal
makePort mode [String filename] _ = fmap Port $ liftIO $ openFile (T.unpack filename) mode
makePort _ [arg] pos = throwError $ TypeError "string" arg pos
makePort _ args pos = throwError $ ArgError 1 (length args) pos

closePort :: [LispVal] -> SourcePos -> EvalResult LispVal
closePort [Port port] _ = liftIO $ hClose port >> return Undefined
closePort [arg] pos = throwError $ TypeError "port" arg pos
closePort args pos = throwError $ ArgError 1 (length args) pos

-- readProc :: [LispVal] -> EvalResult LispVal
-- readProc [] = readProc [Port stdin]
-- readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
-- readProc args = throwError $ ArgError 1 (length args)

writeProc :: [LispVal] -> SourcePos -> EvalResult LispVal
writeProc [obj] pos = writeProc [obj, Port stdout] pos
writeProc [obj, Port port] _ = liftIO $ hPrint port obj >> return Undefined
writeProc args pos = throwError $ ArgError 2 (length args) pos

builtinEnv :: [Map.Map T.Text LispVal]
builtinEnv = [Map.fromList $ map toFunc builtins]
  where
    toFunc (ident, f) = (ident, Func (InternalFn f))

-- these are builtin functions according to the r7rs standard:
-- https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html#TAG:__tex2page_chap_6
builtins :: [(T.Text, [LispVal] -> SourcePos -> EvalResult LispVal)]
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
    -- actually derived expression
    ("and", andOp),
    ("or", orOp),
    -- very basic io functionalities
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    -- ("read", readProc),
    ("write", writeProc)
  ]
