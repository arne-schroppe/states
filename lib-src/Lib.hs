module Lib (
  allCombinations
) where

import Language.States.Types
import Language.States.Parser (parse)

import Data.Maybe (maybeToList)
import Data.List (intersperse)
import Control.Monad (void)


allCombinations :: String -> [String]
allCombinations src =
  let expr = parse src >>= denormalise in
  case expr of
    Left err   -> [err]
    Right expr -> let combs = combinations expr in
                  map prettyPrint combs

-- This step inlines all variable declarations
denormalise :: Expr -> Either String Expr
denormalise expr = denorm [] expr
  where
    denorm :: [(String, Expr)] -> Expr -> Either String Expr
    denorm decls exp =
      case exp of
        EDecl ident declExpr bodyExpr -> denorm decls declExpr >>= \e -> denorm ((ident, e):decls) bodyExpr
        EVariable ident               -> findDecl ident decls
        ETuple es                     -> mapM (denorm decls) es >>= Right . ETuple
        EVariant opts                 -> do denormOpts <- mapM (denormVarOpt decls) opts
                                            return $ EVariant denormOpts

    denormVarOpt :: [(String, Expr)] -> EVarOption -> Either String EVarOption
    denormVarOpt decls (EVarOpt i Nothing)  = Right $ EVarOpt i Nothing
    denormVarOpt decls (EVarOpt i (Just e)) = denorm decls e >>= \de -> Right $ EVarOpt i (Just de)

    findDecl :: String -> [(String, Expr)] -> Either String Expr
    findDecl ident []           = Left $ "Unknown variable '" ++ ident ++ "'"
    findDecl ident ((i,e):rest) = if ident == i then Right e else findDecl ident rest

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

