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
parse src = case Parsec.parse expression "(error)" src of
  Left e  -> Left $ show e
  Right r -> Right r

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

declaration :: Parser Expr
declaration = do
  void $ lexeme $ string "let"
  ident <- lexeme $ varName
  void $ lexeme $ char '='
  declExpr <- expression
  void $ lexeme $ char ';'
  nextExpr <- expression
  return $ EDecl ident declExpr nextExpr

variantOption :: Parser EVarOption
variantOption = do
  ident <- lexeme $ symbol
  expr <- optionMaybe $ expression
  return $ EVarOpt ident expr

symbol :: Parser String
symbol = do
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

