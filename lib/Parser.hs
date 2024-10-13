{-# LANGUAGE OverloadedStrings #-}

module Parser (readExprs) where

import qualified Data.Text as T
import Text.Parsec.Prim (ParsecT)
import Text.ParserCombinators.Parsec hiding (spaces)
import Utils

sourcePos :: (Monad m) => ParsecT s u m SourcePos
sourcePos = statePos <$> getParserState

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

parseAtom :: Parser LispVal
parseAtom = do
  pos <- sourcePos
  x <- letter <|> symbol
  xs <- many $ letter <|> digit <|> symbol
  return $ Atom (T.pack $ x : xs) pos

parseNumber :: Parser LispVal
-- same as: liftM (Number . read) $ many1 digit, because liftM <=> fmap, fmap <=> <$>
parseNumber = do
  n <- many1 digit
  return $ Number (read n)

parseBasicList :: SourcePos -> Parser LispVal
parseBasicList pos = do
  elems <- sepBy parseExpr spaces
  return $ List elems pos

parseDottedList :: SourcePos -> Parser LispVal
parseDottedList pos = do
  elems <- endBy parseExpr spaces
  _ <- char '.' >> spaces
  last' <- parseExpr
  return $ cons elems last' pos
  where
    -- tries to combine operands into proper list, if not then is improper (dotted)
    cons :: [LispVal] -> LispVal -> SourcePos -> LispVal
    cons car (List [] _) _ = List car pos
    cons car (List cdr _) _ = List (car ++ cdr) pos
    cons car (DottedList xs last_' _) _ = case cons xs last_' pos of
      List cdr _ -> List (car ++ cdr) pos
      DottedList xs' last_'' _ -> DottedList (car ++ xs') last_'' pos
      _ -> error "unreachable cons can only return lists"
    cons car cdr _ = DottedList car cdr pos

parseLists :: Parser LispVal
parseLists = do
  pos <- sourcePos
  _ <- char '('
  l <- try (parseBasicList pos) <|> parseDottedList pos
  _ <- char ')'
  return l

parseQuote :: Parser LispVal
parseQuote = do
  pos <- sourcePos
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote" pos, x] pos

parseExpr :: Parser LispVal
parseExpr = parseString <|> parseBool <|> parseAtom <|> parseNumber <|> parseLists <|> parseQuote

parseAll :: Parser [LispVal]
parseAll = many (skipMany spaces *> parseExpr <* skipMany spaces) <* eof

readExprs :: String -> String -> Either SchemeError [LispVal]
readExprs input filename = case parse parseAll filename input of
  Left err -> Left $ Parse err
  Right val -> return val
