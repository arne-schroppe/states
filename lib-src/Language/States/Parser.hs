module Language.States.Parser (
  Language.States.Parser.parse
, testParse
, parseFilters
) where

import Language.States.Types

import Control.Monad (void)
import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Char


parse :: String -> Either String Expr
parse src = case Parsec.parse parseAll "Parse error in source" src of
  Left e  -> Left $ show e
  Right r -> Right r

parseFilters :: String -> Either String [ExprFilter]
parseFilters src = case Parsec.parse (option [] filterBlockContent) "Parse error in extra-filters" src of
  Left e  -> Left $ show e
  Right r -> Right r

parseAll :: Parser Expr
parseAll = do
  whitespace
  expr <- filteredExpr
  eof
  return expr

filteredExpr :: Parser Expr
filteredExpr = do
  expr <- expression
  filters <- option [] filterBlock
  return $ EFiltered expr filters

expression :: Parser Expr
expression = declaration <|> variable <|> tuple <|> variant

tuple :: Parser Expr
tuple = do
  void $ lexeme $ char '('
  exprs <- lexeme $ sepBy1 expression (lexeme $ char ',')
  void $ lexeme $ char ')'
  if length exprs == 1
    then return $ head exprs
    else return $ ETuple exprs

variant :: Parser Expr
variant = do
  vars <- lexeme $ sepBy1 variantOption (lexeme $ char '|')
  return $ EVariant vars

variantOption :: Parser VariantOption
variantOption = do
  ident <- lexeme $ symbol
  expr <- optionMaybe $ expression
  return $ EVarOpt ident expr

declaration :: Parser Expr
declaration = do
  void $ try (lexeme $ keywordLet)
  ident <- lexeme $ varName
  void $ lexeme $ char '='
  declExpr <- filteredExpr
  void $ lexeme $ char ';'
  nextExpr <- expression
  return $ EDecl ident declExpr nextExpr

keywordLet :: Parser String
keywordLet = keyword "let"

keyword :: String -> Parser String
keyword s = do
  void $ string s
  notFollowedBy alphaNum
  return s

filterBlock :: Parser [ExprFilter]
filterBlock = do
  void $ lexeme $ char '['
  filters <- filterBlockContent
  void $ lexeme $ char ']'
  return $ filters

filterBlockContent :: Parser [ExprFilter]
filterBlockContent = lexeme $ sepBy exprFilter (lexeme $ char ',')

exprFilter :: Parser ExprFilter
exprFilter = do
  filterKeyword <- lexeme $ symbol -- Not really a symbol, but the parser is good enough
  pat <- pattern
  filterType <- keywordToFilter filterKeyword
  return $ EFilter filterType pat

keywordToFilter :: String -> Parser FilterType
keywordToFilter "remove"    = return FTRemove
keywordToFilter "only"      = return FTOnly
keywordToFilter "highlight" = return FTHighlight
keywordToFilter s           = unexpected $ "filter type '" ++ s ++ "'"

pattern :: Parser Pattern
pattern = do
  void $ lexeme $ char '('
  p <- patternTupleContent <|> patternVariant
  void $ lexeme $ char ')'
  return p

patternExpr :: Parser Pattern
patternExpr = patternTuple <|> patternVariant

patternTuple :: Parser Pattern
patternTuple = do
  void $ lexeme $ char '('
  pat <- patternTupleContent
  void $ lexeme $ char ')'
  return pat

patternTupleContent :: Parser Pattern
patternTupleContent = do
  pats <- lexeme $ sepBy1 patternExpr (lexeme $ char ',')
  if length pats == 1
    then return $ head pats
    else return $ PTuple pats

patternVariant :: Parser Pattern
patternVariant = do
  identPat <- identPattern
  pat <- optionMaybe $ patternExpr
  return $ PVariant identPat pat

identPattern :: Parser IdentPattern
identPattern = patternWildcard <|> patternSymbol

patternWildcard :: Parser IdentPattern
patternWildcard = (lexeme $ char '_') >> return IPWildcard

patternSymbol :: Parser IdentPattern
patternSymbol = do
  sym <- lexeme $ symbol
  return $ IPIdent sym

symbol :: Parser String
symbol = (quotedString '"') <|> (quotedString '\'') <|> regularSymbol


quotedString :: Char -> Parser String
quotedString quoteChar = do
  void $ lexeme $ char quoteChar
  s <- stringContent
  void $ char quoteChar
  return s

stringContent :: Parser String
stringContent = many $ alphaNum <|> char ' '

regularSymbol :: Parser String
regularSymbol = do
  notFollowedBy $ keywordLet
  first <- lower
  rest  <- identRest
  return $ first:rest

variable :: Parser Expr
variable = do
  ident <- varName
  return $ EVariable ident

varName :: Parser String
varName = do
  first <- upper
  rest  <- identRest
  return $ first:rest

identRest :: Parser String
identRest = many $ letter <|> digit <|> oneOf "_-"

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x


-- Debugging
testParse :: String -> IO ()
testParse = Parsec.parseTest parseAll

