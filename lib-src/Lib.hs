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
  let src' = removeComments src in
  let expr = parse src' >>= denormalise in
  case expr of
    Left err -> [err]
    Right expr -> -- [show expr]
                  let combs = filteredCombinations expr in
                  map prettyPrint combs

-- TODO figure out which regex library is good and use regexes instead
removeComments :: String -> String
removeComments src =
  remCom False "" src
  where
    remCom :: Bool -> String -> String -> String
    remCom _     processed []          = reverse processed
    remCom False processed ('#':rest)  = remCom True processed rest
    remCom True  processed ('\n':rest) = remCom False processed rest
    remCom True  processed (_:rest)    = remCom True processed rest
    remCom False processed (c:rest)    = remCom False (c:processed) rest

-- This step inlines all variable declarations
denormalise :: FilteredExpr -> Either String FilteredExpr
denormalise (FExpr expr filters) = do
    e <- denorm [] expr
    return $ FExpr e filters
  where
    denorm :: [(String, Expr)] -> Expr -> Either String Expr
    denorm decls exp =
      case exp of
        EDecl ident declExpr bodyExpr -> do e <- denorm decls declExpr; denorm ((ident, e):decls) bodyExpr
        EVariable ident               -> findDecl ident decls
        ETuple es                     -> do des <- mapM (denorm decls) es; return $ ETuple des
        EVariant opts                 -> do denormOpts <- mapM (denormVarOpt decls) opts
                                            return $ EVariant denormOpts

    denormVarOpt :: [(String, Expr)] -> VariantOption -> Either String VariantOption
    denormVarOpt decls (EVarOpt i Nothing)  = Right $ EVarOpt i Nothing
    denormVarOpt decls (EVarOpt i (Just e)) = do de <- denorm decls e; return $ EVarOpt i (Just de)

    findDecl :: String -> [(String, Expr)] -> Either String Expr
    findDecl ident []           = Left $ "Unknown variable '" ++ ident ++ "'"
    findDecl ident ((i,e):rest) = if ident == i then Right e else findDecl ident rest

filteredCombinations :: FilteredExpr -> [Value]
filteredCombinations (FExpr expr filters) = combinations expr

combinations :: Expr -> [Value]
combinations expr = case expr of
  ETuple es -> map VTuple $ listCombinations $ map combinations es
  EVariant es -> concatMap mappingFunc es
    where
      mappingFunc :: VariantOption -> [Value]
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

