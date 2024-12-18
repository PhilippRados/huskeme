{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Builtins (builtinEnv) where

import qualified Control.Exception as E
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.List (find)
import qualified Data.Map as Map
import qualified Data.Text as T
import Interpreter.Eval (applyOp, runWithEnv)
import Parser (readExpr)
import System.IO
import Utils

unpackNum :: LispVal -> Loc -> EvalResult Integer
unpackNum (Number n) _ = return n
unpackNum x loc = throwError $ TypeError "number" x loc

unpackString :: LispVal -> Loc -> EvalResult T.Text
unpackString (String s) _ = return s
unpackString x loc = throwError $ TypeError "string" x loc

binOp :: (b -> LispVal) -> (LispVal -> Loc -> EvalResult a) -> ([a] -> b) -> [LispVal] -> Loc -> EvalResult LispVal
binOp _ _ _ [_] loc = throwError $ ArgError 2 1 loc
binOp _ _ _ [] loc = throwError $ ArgError 2 0 loc
binOp pack unpack foldOp args loc = fmap (pack . foldOp) (mapM (`unpack` loc) args)

arithOp :: (Integer -> Integer -> Integer) -> [LispVal] -> Loc -> EvalResult LispVal
arithOp op = binOp Number unpackNum (foldl1 op)

-- allows folding values to boolean types instead of only the lists type
foldComp :: (a -> a -> Bool) -> [a] -> Bool
foldComp _ [] = error "unreachable because binOp checks for enough args"
foldComp op (x' : xs') = innerFold True x' xs'
  where
    innerFold acc _ [] = acc
    innerFold acc prev (x : xs) = innerFold (op prev x && acc) x xs

numCompOp :: (Integer -> Integer -> Bool) -> [LispVal] -> Loc -> EvalResult LispVal
numCompOp op = binOp Bool unpackNum $ foldComp op

strCompOp :: (T.Text -> T.Text -> Bool) -> [LispVal] -> Loc -> EvalResult LispVal
strCompOp op = binOp Bool unpackString $ foldComp op

car :: [LispVal] -> Loc -> EvalResult LispVal
car [List (x : _) _] _ = return x
car [DottedList (x : _) _ _] _ = return x
car [arg] loc = throwError $ TypeError "list" arg loc
car args loc = throwError $ ArgError 1 (length args) loc

cdr :: [LispVal] -> Loc -> EvalResult LispVal
cdr [List (_ : xs) _] loc = return $ List xs loc
cdr [DottedList [_] x _] _ = return x
cdr [DottedList (_ : xs) x _] loc = return $ DottedList xs x loc
cdr [arg] loc = throwError $ TypeError "list" arg loc
cdr args loc = throwError $ ArgError 1 (length args) loc

cons :: [LispVal] -> Loc -> EvalResult LispVal
cons [x1, List [] _] loc = return $ List [x1] loc
cons [x1, List xs _] loc = return $ List (x1 : xs) loc
cons [x1, DottedList xs last_ _] loc = return $ DottedList (x1 : xs) last_ loc
cons [x1, x2] loc = return $ DottedList [x1] x2 loc
cons args loc = throwError $ ArgError 2 (length args) loc

-- eq and eqv can be the same according to r7rs:
-- It must always return #f when eqv? also would,
-- but may return #f in some cases where eqv? would return #t
eqv :: [LispVal] -> Loc -> EvalResult LispVal
eqv [Number a, Number b] _ = return $ Bool $ a == b
eqv [Bool a, Bool b] _ = return $ Bool $ a == b
eqv [Atom a _, Atom b _] _ = return $ Bool $ a == b -- can only happen if symbol because lookup extracts stored value of variable
eqv [String a, String b] _ = return $ Bool $ a == b
eqv [List [] _, List [] _] _ = return $ Bool True
eqv [_, _] _ = return $ Bool False
eqv args loc = throwError $ ArgError 2 (length args) loc

-- same as eqv but can be used for lists as well
equal :: [LispVal] -> Loc -> EvalResult LispVal
equal [List xs _, List ys _] loc = equalLists xs ys loc
equal [DottedList xs x _, DottedList ys y _] loc = equalLists (xs ++ [x]) (ys ++ [y]) loc
equal args loc = eqv args loc

equalLists :: [LispVal] -> [LispVal] -> Loc -> EvalResult LispVal
equalLists xs ys loc = equal_values >>= (\eq -> return $ Bool $ same_length && eq)
  where
    same_length = length xs == length ys
    equal_values = do
      values <- mapM (\(a, b) -> equal [a, b] loc) $ zip xs ys
      return $ all (== Bool True) values

andOp :: [LispVal] -> Loc -> EvalResult LispVal
andOp [] _ = return $ Bool True
andOp args _ = return $ case find (== Bool False) args of
  Just falsy -> falsy
  Nothing -> last args

orOp :: [LispVal] -> Loc -> EvalResult LispVal
orOp [] _ = return $ Bool False
orOp args _ = return $ case find (/= Bool False) args of
  Just truthy -> truthy
  Nothing -> last args

-------------------- IO Functions --------------------

ioThrows :: (IO a) -> Loc -> EvalResult a
ioThrows f loc = do
  result <- liftIO (E.try f)
  case result of
    Left err -> throwError $ IOErr err loc
    Right x -> return x

makePort :: IOMode -> [LispVal] -> Loc -> EvalResult LispVal
makePort mode [String filename] loc = do
  handle <- ioThrows (openFile (T.unpack filename) mode) loc
  return $ Port handle
makePort _ [arg] loc = throwError $ TypeError "string" arg loc
makePort _ args loc = throwError $ ArgError 1 (length args) loc

closePort :: [LispVal] -> Loc -> EvalResult LispVal
closePort [Port port] loc = ioThrows (hClose port) loc >> return Undefined
closePort [arg] loc = throwError $ TypeError "port" arg loc
closePort args loc = throwError $ ArgError 1 (length args) loc

readProc :: [LispVal] -> Loc -> EvalResult LispVal
readProc [] loc = readProc [Port stdin] loc
readProc [Port port] loc = do
  expr <- ioThrows (hGetLine port) loc
  lift $ except (readExpr expr (handleName port))
readProc [arg] loc = throwError $ TypeError "port" arg loc
readProc args loc = throwError $ ArgError 1 (length args) loc

writeProc :: [LispVal] -> Loc -> EvalResult LispVal
writeProc [obj] loc = writeProc [obj, Port stdout] loc
writeProc [obj, Port port] loc = ioThrows (hPrint port obj) loc >> return Undefined
writeProc [_, arg] loc = throwError $ TypeError "port" arg loc
writeProc args loc = throwError $ ArgError 2 (length args) loc

load :: [LispVal] -> Loc -> EvalResult LispVal
load [String filename] loc = do
  input <- ioThrows (readFile s_filename) loc
  _ <- runWithEnv input s_filename
  return Undefined
  where
    s_filename = T.unpack filename
load [arg] loc = throwError $ TypeError "port" arg loc
load args loc = throwError $ ArgError 2 (length args) loc

-- INFO: first arg must be callable, last arg must be list, prevs can be single values
applyProc :: [LispVal] -> Loc -> EvalResult LispVal
applyProc (f : args) loc
  | null args = throwError $ ArgError 2 (length args) loc
  | otherwise = case reverse args of
      (List last_ _ : xs) -> applyOp f (xs ++ last_) loc
      _ -> throwError $ TypeError "list" (head args) loc
applyProc args loc = throwError $ ArgError 2 (length args) loc

builtinEnv :: Env
builtinEnv =
  let len = length builtins
      refs = zip (map fst builtins) [0 .. len]
      vals = zip [0 .. len] $ map (\b -> (toFunc . snd $ b, False)) builtins
      toFunc f = Func (InternalFn f)
   in Env {envRefs = Map.fromList refs, envVals = Map.fromList vals}

-- these are builtin functions according to the r7rs standard:
-- https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html#TAG:__tex2page_chap_6
builtins :: [(T.Text, [LispVal] -> Loc -> EvalResult LispVal)]
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
    ("read", readProc),
    ("write", writeProc),
    ("load", load),
    ("apply", applyProc)
  ]
