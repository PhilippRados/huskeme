{-# LANGUAGE OverloadedStrings #-}

module Parser (readExprs) where

import qualified Data.Text as T
import Error
import Text.ParserCombinators.Parsec hiding (spaces)
import Types

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
  cons elems <$> parseExpr
  where
    -- tries to combine operands into proper list, if not then is improper (dotted)
    cons :: [LispVal] -> LispVal -> LispVal
    cons car (List []) = List car
    cons car (List cdr) = List $ car ++ cdr
    cons car (DottedList xs last_') = case cons xs last_' of
      List cdr -> List $ car ++ cdr
      DottedList xs' last_'' -> DottedList (car ++ xs') last_''
      _ -> error "unreachable cons can only return lists"
    cons car cdr = DottedList car cdr

parseLists :: Parser LispVal
parseLists = do
  _ <- char '('
  l <- try parseBasicList <|> parseDottedList
  _ <- char ')'
  return l

parseQuote :: Parser LispVal
parseQuote = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseString <|> parseBool <|> parseAtom <|> parseNumber <|> parseLists <|> parseQuote

parseAll :: Parser [LispVal]
parseAll = many (skipMany space *> parseExpr <* skipMany space) <* eof

readExprs :: T.Text -> Either SchemeError [LispVal]
readExprs input = case parse parseAll "<stdin>" (T.unpack input) of
  Left err -> Left $ Parse err
  Right val -> return val
