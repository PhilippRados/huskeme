{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( LispVal (..),
    parseExpr,
    readExpr,
  )
where

import Data.Text as T
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Nil
  | Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Bool Bool
  deriving (Eq)

instance Show LispVal where show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal val =
  case val of
    (Atom atom) -> atom
    (String str) -> T.concat ["\"", str, "\""]
    (Number num) -> T.pack $ show num
    (Bool True) -> "#t"
    (Bool False) -> "#f"
    Nil -> "Nil"
    ------------------------------------ same as: map showVal contents
    (List contents) -> T.concat ["(", T.unwords $ showVal <$> contents, ")"]

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  val <- char 't' <|> char 'f'
  return $ Bool $ case val of
    't' -> True
    'f' -> False
    _ -> error "unreachable: monad shortcircuits before"

parseNil :: Parser LispVal
parseNil = string "nil" >> return Nil

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  literal <- many $ noneOf "\""
  _ <- char '"'
  return $ String $ T.pack literal

parseAtom :: Parser LispVal
parseAtom = do
  x <- letter
  xs <- many $ letter <|> digit
  return $ Atom $ T.pack $ x : xs

parseNumber :: Parser LispVal
-- same as: liftM (Number . read) $ many1 digit, because liftM <=> fmap, fmap <=> <$>
parseNumber = Number . read <$> many1 digit

parseExpr :: Parser LispVal
parseExpr = parseString <|> parseBool <|> parseNil <|> parseAtom <|> parseNumber

readExpr :: T.Text -> Either ParseError LispVal
readExpr input = parse parseExpr "default_file.ls" (T.unpack input)
