{-# LANGUAGE OverloadedStrings #-}

module Parser (readExprs, readExpr) where

import qualified Data.Text as T
import Text.Parsec.Prim (ParsecT)
import Text.ParserCombinators.Parsec hiding (spaces)
import Utils

sourcePos :: (Monad m) => String -> ParsecT s u m Loc
sourcePos input = do
  pos <- statePos <$> getParserState
  return Loc {locPos = pos, locInput = input}

-- r7rs: Comments are treated exactly like whitespace
comment :: Parser ()
comment = do
  _ <- try lineComment <|> try blockComment
  return ()
  where
    lineComment = do
      _ <- char ';'
      _ <- manyTill anyChar (try newline)
      return ()
    blockComment = do
      _ <- string "#|"
      _ <- manyTill anyChar (try (string "|#"))
      return ()

spaces :: Parser ()
spaces = skipMany1 (skipMany1 space <|> comment)

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  val <- char 't' <|> char 'f'
  let val' =
        ( case val of
            't' -> True
            'f' -> False
            _ -> error "unreachable: monad shortcircuits before"
        )
  return $ Bool val'

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  literal <- many $ noneOf "\""
  _ <- char '"'
  return $ String (T.pack literal)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseAtom :: String -> Parser LispVal
parseAtom input = do
  pos <- sourcePos input
  x <- letter <|> symbol
  xs <- many $ letter <|> digit <|> symbol
  return $ Atom (T.pack $ x : xs) pos

parseNumber :: String -> Parser LispVal
parseNumber input = do
  signage <- optionMaybe $ try (char '-' <|> char '+')
  n <- try $ many1 digit
  _ <- notFollowedBy (parseAtom input) -- (-12 should be number but, -12foo should be atom)
  return $
    Number
      ( case signage of
          Just '-' -> -(read n)
          _ -> read n
      )

parseBasicList :: String -> Loc -> Parser LispVal
parseBasicList input pos = do
  elems <- sepBy (parseExpr input) spaces
  return $ List elems pos

parseDottedList :: String -> Loc -> Parser LispVal
parseDottedList input pos = do
  elems <- endBy (parseExpr input) spaces
  _ <- char '.' >> spaces
  last' <- parseExpr input
  return $ cons elems last' pos
  where
    -- tries to combine operands into proper list, if not then is improper (dotted)
    cons :: [LispVal] -> LispVal -> Loc -> LispVal
    cons car (List [] _) _ = List car pos
    cons car (List cdr _) _ = List (car ++ cdr) pos
    cons car (DottedList xs last_' _) _ = case cons xs last_' pos of
      List cdr _ -> List (car ++ cdr) pos
      DottedList xs' last_'' _ -> DottedList (car ++ xs') last_'' pos
      _ -> error "unreachable cons can only return lists"
    cons car cdr _ = DottedList car cdr pos

parseLists :: String -> Parser LispVal
parseLists input = do
  loc <- sourcePos input
  _ <- char '('
  l <- try (parseBasicList input loc) <|> parseDottedList input loc
  _ <- char ')'
  return l

parseQuote :: String -> Parser LispVal
parseQuote input = do
  loc <- sourcePos input
  _ <- char '\''
  x <- parseExpr input
  return $ List [Atom "quote" loc, x] loc

-- NOTE: needs input as argument to also have full input at every error location
parseExpr :: String -> Parser LispVal
parseExpr input =
  parseString
    <|> parseBool
    <|> try (parseNumber input) -- can fail because of signage
    <|> parseAtom input
    <|> parseLists input
    <|> parseQuote input

parseAll :: String -> Parser [LispVal]
parseAll input = many (skipMany spaces *> parseExpr input <* skipMany spaces) <* eof

readE :: (String -> Parser a) -> String -> String -> Either SchemeError a
readE p input filename = case parse (p input) filename input of
  Left err -> Left $ Parse err input
  Right val -> return val

readExpr :: String -> String -> Either SchemeError LispVal
readExpr = readE parseExpr

readExprs :: String -> String -> Either SchemeError [LispVal]
readExprs = readE parseAll
