module Lib
( allCombinations
) where

import Language.States.Types
import Data.Maybe (maybeToList)
import Data.List (intersperse)

parseExpr :: String -> Maybe Expr
parseExpr src = Just $ ETuple[ EVariant [EVarOpt "a" Nothing, EVarOpt "x" Nothing], EVariant [EVarOpt "b" (Just (EVariant [EVarOpt "c" Nothing, EVarOpt "d" $ Just (ETuple [EVariant [EVarOpt "z" Nothing, EVarOpt "w" Nothing]])]))]] -- Tuple [Symbol "a", Variant [Symbol "b", Symbol "c"]]

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
  VTuple vals  -> "(" ++ concatMap id (intersperse ", " (map prettyPrint vals)) ++ ")"
  VVariant s e -> s ++ maybe "" (\x -> " " ++ prettyPrint x) e

allCombinations :: String -> [String]
allCombinations src =
  let expr = parseExpr src in
  let combs = concatMap combinations (maybeToList expr) in
  map prettyPrint combs

