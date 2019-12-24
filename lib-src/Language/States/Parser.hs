module Language.States.Parser (
  Language.States.Parser.parse
)
where

import Language.States.Types

import Control.Monad (void)
import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

parse :: String -> Either String Expr
parse src = case Parsec.parse allInput "error" src of
  Left e  -> Left $ show e
  Right r -> Right r

allInput :: Parser Expr
allInput = do
  expr <- expression
  eof
  return expr

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
  declExpr <- expression
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

pattern :: Parser Pattern
pattern = do
  void $ lexeme $ char '%'
  void $ lexeme $ char '('
  p <- patternTupleContent <|> patternVariant
  void $ lexeme $ char ')'
  return p

patternTuple :: Parser Pattern
patternTuple = do
  void $ lexeme $ char '('
  pat <- patternTupleContent
  void $ lexeme $ char ')'
  return pat

patternTupleContent :: Parser Pattern
patternTupleContent = do
  pats <- lexeme $ sepBy1 pattern (lexeme $ char ',')
  if length pats == 1
    then return $ head pats
    else return $ PTuple pats

patternVariant :: Parser Pattern
patternVariant = do
  identPat <- identPattern
  pat <- optionMaybe $ pattern
  return $ PVariant identPat pat

identPattern :: Parser IdentPattern
identPattern = try patternWildcard <|> try patternSymbol

patternWildcard :: Parser IdentPattern
patternWildcard = (lexeme $ char '_') >> return IPWildcard

patternSymbol :: Parser IdentPattern
patternSymbol = do
  sym <- lexeme $ symbol
  return $ IPIdent sym

symbol :: Parser String
symbol = do
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


