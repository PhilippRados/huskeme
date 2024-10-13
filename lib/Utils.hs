{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils (LispVal (..), EvalResult, InternalFn (..), Env, EvalError (..), printError, SchemeError (..)) where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Void
import Error.Diagnose
import Error.Diagnose.Compat.Parsec
import System.IO (Handle)
import Text.Parsec

type Env = Map.Map T.Text LispVal

-- NOTE: I know this is supposed to be an anti-pattern, but I prefer my errors to be explicit
-- but I don't like that this makes basically every function impure, I would like to separate out the IO more

type EvalResult a = StateT [Env] (ExceptT EvalError IO) a

data LispVal
  = Atom T.Text SourcePos
  | -- proper list that terminates with empty list: (1 . (2 . (3 . ()))) => (1 2 3)
    List [LispVal] SourcePos
  | -- improper list that doesn't terminate with empty list: (1 . (2 . 3)) => (1 2 . 3)
    DottedList [LispVal] LispVal SourcePos
  | Number Integer
  | String T.Text
  | Bool Bool
  | Func InternalFn
  | Lambda
      { lambdaParams :: [T.Text],
        lambdaVarargs :: Maybe T.Text,
        lambdaBody :: [LispVal]
      }
  | Port Handle
  | Undefined

instance Eq LispVal where
  Atom t1 _ == Atom t2 _ = t1 == t2
  List l1 _ == List l2 _ = l1 == l2
  DottedList l1 last1 _ == DottedList l2 last2 _ = l1 == l2 && last1 == last2
  Number n1 == Number n2 = n1 == n2
  String n1 == String n2 = n1 == n2
  Bool n1 == Bool n2 = n1 == n2
  Port h1 == Port h2 = h1 == h2
  _ == _ = False

newtype InternalFn = InternalFn {fn :: [LispVal] -> SourcePos -> EvalResult LispVal}

instance Show LispVal where
  show (Atom atom _) = T.unpack atom
  show (String str) = "\"" ++ T.unpack str ++ "\""
  show (Number num) = show num
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List contents _) = "(" ++ unwords (show <$> contents) ++ ")"
  show (DottedList contents last_ _) = "(" ++ unwords (show <$> contents) ++ " . " ++ show last_ ++ ")"
  show (Func _) = "<builtin-function>"
  show (Lambda {lambdaParams = args, lambdaVarargs = va, lambdaBody = _}) = "<lambda (" ++ unwords (map show args) ++ varargs va ++ ")>"
    where
      varargs (Just v) = " . " ++ T.unpack v
      varargs Nothing = ""
  show Undefined = "<undefined>"
  show (Port h) =
    let portname = takeWhile (/= '}') $ dropWhile (/= ' ') $ show h
     in "<port:" ++ portname ++ ">"

--------------------- Error Stuff ---------------------

data SchemeError = Parse ParseError | Eval EvalError deriving (Eq, Show) -- Show only used in tests

data EvalError
  = TypeError T.Text LispVal SourcePos
  | ArgError Int Int SourcePos
  | UnboundVar T.Text SourcePos
  deriving (Eq, Show)

instance HasHints Void String where hints _ = mempty

printError :: SchemeError -> String -> String -> IO ()
printError (Parse e) input filename = printDiag diag''
  where
    diag' = errorDiagnosticFromParseError Nothing "parse error" Nothing e
    diag'' = addFile diag' filename input
printError (Eval e) input filename = printDiag diag''
  where
    (msg, pos) = errorData e
    diag' = addFile mempty filename input
    diag'' = addReport diag' (Err Nothing msg pos [])

printDiag :: Diagnostic String -> IO ()
printDiag = printDiagnostic stderr WithUnicode (TabSize 4) defaultStyle

errorData :: EvalError -> (String, [(Position, Marker String)])
errorData (TypeError expected got pos) = ("mismatched types", [(convertPos pos, This ("this call expected an arg of type: " ++ T.unpack expected ++ ", but got: " ++ getKind got))])
errorData (ArgError expected got pos) = ("mismatched number of arguments", [(convertPos pos, This ("this call expected: " ++ show expected ++ " arg(s), but got " ++ show got))])
errorData (UnboundVar name pos) = ("unbound variable " ++ T.unpack name, [(convertPos pos, This "is this even defined?")])

getKind :: LispVal -> String
getKind (Atom {}) = "atom"
getKind (String {}) = "string"
getKind (Number {}) = "number"
getKind (Bool {}) = "boolean"
getKind (List {}) = "proper list"
getKind (DottedList {}) = "improper list"
getKind (Func {}) = "function"
getKind (Lambda {}) = "lambda"
getKind Undefined = "undefined"
getKind (Port {}) = "port"

convertPos :: SourcePos -> Position
convertPos pos =
  Position
    { begin = (sourceLine pos, sourceColumn pos),
      end = (sourceLine pos, sourceColumn pos + 1),
      file = sourceName pos
    }
