module Error (EvalError (..), formatError, SchemeError (..)) where

import Data.Text as T (Text, pack, replace, unpack)
import Text.Parsec
import Text.Parsec.Error (errorMessages, showErrorMessages)

data SchemeError = Parse ParseError | Eval EvalError deriving (Show, Eq)

data EvalError = TypeError T.Text | ArgError Int Int | BasicError T.Text deriving (Eq)

instance Show EvalError where
  -- TODO: also add received type
  show (TypeError ty) = "mismatched type: expected " ++ T.unpack ty
  show (ArgError expected got) = "mismatched number of arguments: expected " ++ show expected ++ ", got: " ++ show got
  show (BasicError msg) = T.unpack msg

formatError :: SchemeError -> String -> String
formatError (Parse err) input = formatParseError err input
formatError (Eval err) _input = show err

formatEvalError :: EvalError -> String -> String
formatEvalError err input = show err

formatParseError :: ParseError -> String -> String
formatParseError err input =
  let pos = errorPos err
      line = sourceLine pos
      column = sourceColumn pos
      source = sourceName pos
      customMessage =
        showErrorMessages
          "or"
          "unknown parse error"
          "expected"
          "unexpected"
          "end of input"
      msg = replaceNewlines $ drop 1 $ customMessage $ errorMessages err
      markedError = markError line input column
   in highlight "Error: " ++ msg ++ "\n |--> " ++ source ++ ":" ++ show line ++ ":" ++ show column ++ "\n" ++ markedError

markError :: Int -> String -> Int -> String
markError line input column = lineInfo ++ input ++ "\n" ++ marker
  where
    lineInfo = show line ++ "| "
    marker = replicate (column - 1 + length lineInfo) ' ' ++ highlight "^"

replaceNewlines :: String -> String
replaceNewlines input = T.unpack $ replace (T.pack "\n") (T.pack ", ") $ T.pack input

highlight :: String -> String
highlight input = "\x1b[" ++ color ++ ";" ++ bold ++ "m" ++ input ++ "\x1b[0m"
  where
    color = "31" -- RED
    bold = "1"
