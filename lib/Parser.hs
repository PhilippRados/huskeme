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
  = Atom T.Text
  | List [LispVal]
  | DottedList [LispVal] LispVal
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
    ------------------------------------ same as: map showVal contents
    (List contents) -> T.concat ["(", T.unwords $ showVal <$> contents, ")"]
    (DottedList contents last_) -> T.concat ["(", T.unwords $ showVal <$> contents, " . ", showVal last_, ")"]

spaces :: Parser ()
spaces = skipMany1 space

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  val <- char 't' <|> char 'f'
  return $ Bool $ case val of
    't' -> True
    'f' -> False
    _ -> error "unreachable: monad shortcircuits before"

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  literal <- many $ noneOf "\""
  _ <- char '"'
  return $ String $ T.pack literal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
  x <- letter <|> symbol
  xs <- many $ letter <|> digit <|> symbol
  return $ Atom $ T.pack $ x : xs

parseNumber :: Parser LispVal
-- same as: liftM (Number . read) $ many1 digit, because liftM <=> fmap, fmap <=> <$>
parseNumber = Number . read <$> many1 digit

parseBasicList :: Parser LispVal
parseBasicList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  elems <- endBy parseExpr spaces
  _ <- char '.' >> spaces
  DottedList elems <$> parseExpr

parseLists :: Parser LispVal
parseLists = do
  _ <- char '('
  l <- try parseBasicList <|> parseDottedList
  _ <- char ')'
  return l

parseExpr :: Parser LispVal
parseExpr = parseString <|> parseBool <|> parseAtom <|> parseNumber <|> parseLists

readExpr :: T.Text -> Either ParseError LispVal
readExpr input = parse parseExpr "<stdin>" (T.unpack input)
