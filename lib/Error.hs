{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Error (EvalError (..), formatError, SchemeError (..)) where

import Data.Text as T (Text, pack, replace, unpack)
import Data.Void
import Error.Diagnose
import Error.Diagnose.Compat.Parsec
import Text.Parsec
import Text.Parsec.Token

data SchemeError = Parse ParseError | Eval EvalError deriving (Show, Eq)

-- data EvalError
--   = TypeError LispVal LispVal
--   | ArgError LispVal Int Int
--   | BasicError T.Text
--   | UnboundVar T.Text LispVal
--   deriving (Eq)

data EvalError
  = TypeError T.Text
  | ArgError Int Int
  | BasicError T.Text
  | UnboundVar T.Text
  deriving (Eq)

instance HasHints Void String where hints _ = mempty

instance Show EvalError where
  -- TODO: also add received type
  show (TypeError ty) = "mismatched type: expected " ++ T.unpack ty
  show (ArgError expected got) = "mismatched number of arguments: expected " ++ show expected ++ ", got: " ++ show got
  show (BasicError msg) = T.unpack msg
  show (UnboundVar name) = "unbound variable " ++ T.unpack name

formatError :: SchemeError -> String -> String -> IO ()
formatError (Parse e) input filename = printDiagnostic stdout True True 4 defaultStyle diag'
  where
    diag = errorDiagnosticFromParseError Nothing "Parse error" Nothing e
    diag' = addFile diag filename input
formatError (Eval e) _input _ = putStrLn $ show e
