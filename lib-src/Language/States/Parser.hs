module Language.States.Parser (
  Language.States.Parser.parse
)
where

import Language.States.Types

import Control.Monad (void)
import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

parse :: String -> Either ParseError Expr
parse src = Parsec.parse expr "(error)" src

expr :: Parser Expr
expr = tuple <|> variant

tuple :: Parser Expr
tuple = do
  void $ lexeme $ char '('
  es <- lexeme $ sepBy1 expr (lexeme $ char ',')
  void $ lexeme $ char ')'
  return $ ETuple es

variant :: Parser Expr
variant = do
  vars <- lexeme $ sepBy1 variantOption (lexeme $ char '|')
  return $ EVariant vars

variantOption :: Parser EVarOption
variantOption = do
  i <- lexeme $ symbol
  e <- optionMaybe $ expr
  return $ EVarOpt i e

symbol :: Parser String
symbol = do
  first <- lower
  rest  <- many $ letter <|> digit <|> oneOf "_$"
  return $ first:rest

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

