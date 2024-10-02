module Types (LispVal (..), EvalResult, InternalFn (..), Env) where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import Error (EvalError)

type Env = Map.Map T.Text LispVal

type EvalResult a = StateT [Env] (Except EvalError) a

data LispVal
  = Atom T.Text
  | -- proper list that terminates with empty list: (1 . (2 . (3 . ()))) => (1 2 3)
    List [LispVal]
  | -- improper list that doesn't terminate with empty list: (1 . (2 . 3)) => (1 2 . 3)
    DottedList [LispVal] LispVal
  | Number Integer
  | String T.Text
  | Bool Bool
  | Func InternalFn
  | Lambda
      { lambdaParams :: [LispVal],
        lambdaBody :: [LispVal]
      }
  | Undefined
  deriving (Eq)

newtype InternalFn = InternalFn {fn :: [LispVal] -> EvalResult LispVal}

instance Eq InternalFn where
  _ == _ = False

instance Show LispVal where
  show (Atom atom) = T.unpack atom
  show (String str) = "\"" ++ T.unpack str ++ "\""
  show (Number num) = show num
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List contents) = "(" ++ Prelude.unwords (show <$> contents) ++ ")"
  show (DottedList contents last_) = "(" ++ Prelude.unwords (show <$> contents) ++ " . " ++ show last_ ++ ")"
  show (Func _) = "<builtin-function>"
  show (Lambda {lambdaParams = args, lambdaBody = _}) = "<lambda (" ++ unwords (map show args) ++ ")>"
  show Undefined = "<undefined>"
