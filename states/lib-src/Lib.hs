module Lib
( allCombinations
) where

import Language.States.Types
import Data.Maybe (maybeToList)
import Data.List (intersperse)
import Control.Monad (void)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator


ident :: Parser String
ident = many1 $ letter <|> digit <|> oneOf "_$"

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

parseExpr :: String -> Either ParseError Expr
parseExpr src = parse expr "(error)" src

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
  i <- lexeme $ ident
  e <- optionMaybe $ expr
  return $ EVarOpt i e

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x


combinations :: Expr -> [Value]
combinations expr = case expr of
  ETuple es -> map VTuple $ listCombinations $ map combinations es
  EVariant es -> concatMap mappingFunc es
    where
      mappingFunc :: EVarOption -> [Value]
      mappingFunc (EVarOpt s Nothing)  = [VVariant s Nothing]
      mappingFunc (EVarOpt s (Just e)) = let vs = combinations e in
                                         map (\x -> VVariant s $ Just x) vs

listCombinations :: [[a]] -> [[a]]
listCombinations (xs:[])  = map pure xs
listCombinations (xs:xss) = [ a:b | a <- xs, b <- listCombinations xss ]
listCombinations []       = []

prettyPrint :: Value -> String
prettyPrint val = case val of
  VTuple (v:[]) -> prettyPrint v
  VTuple vals   -> "(" ++ concat (intersperse ", " (map prettyPrint vals)) ++ ")"
  VVariant s e  -> s ++ maybe "" (\x -> " " ++ prettyPrint x) e

allCombinations :: String -> [String]
allCombinations src =
  let result = parseExpr src in
  case result of
    Left err   -> [show err]
    Right expr -> let combs = combinations expr in
                  map prettyPrint combs

