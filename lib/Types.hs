module Types (LispVal (..), EvalResult, InternalFn (..), Env) where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import Error (EvalError)
import System.IO (Handle)

type Env = Map.Map T.Text LispVal

-- NOTE: I know this is supposed to be an anti-pattern, but I prefer my errors to be explicit
-- but I don't like that this makes basically every function impure, I would like to separate out the IO more
type EvalResult a = StateT [Env] (ExceptT EvalError IO) a

-- data LispValLoc = LispValLoc
--   { kind :: LispVal,
--     loc :: Location
--   }

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
      { lambdaParams :: [T.Text],
        lambdaBody :: [LispVal]
      }
  | Port Handle
  | Undefined
  deriving (Eq)

-- data Location

newtype InternalFn = InternalFn {fn :: [LispVal] -> EvalResult LispVal}

instance Eq InternalFn where
  _ == _ = False

instance Show LispVal where
  show (Atom atom) = T.unpack atom
  show (String str) = "\"" ++ T.unpack str ++ "\""
  show (Number num) = show num
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List contents) = "(" ++ unwords (show <$> contents) ++ ")"
  show (DottedList contents last_) = "(" ++ unwords (show <$> contents) ++ " . " ++ show last_ ++ ")"
  show (Func _) = "<builtin-function>"
  show (Lambda {lambdaParams = args, lambdaBody = _}) = "<lambda (" ++ unwords (map show args) ++ ")>"
  show Undefined = "<undefined>"
  show (Port h) =
    let portname = takeWhile (/= '}') $ dropWhile (/= ' ') $ show h
     in "<port:" ++ portname ++ ">"
