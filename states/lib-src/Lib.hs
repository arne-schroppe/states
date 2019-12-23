module Lib
( allCombinations
) where

import Language.States.Types
import Data.Maybe (maybeToList)
-- import Text.ParserCombinators.Parsec
-- import Text.ParserCombinators.Parsec.Expr



parse :: String -> Maybe Expr
parse src = Just $ ETuple[ EVariant [EVarOpt "a" Nothing, EVarOpt "x" Nothing], EVariant [EVarOpt "b" (Just (EVariant [EVarOpt "c" Nothing, EVarOpt "d" Nothing]))]] -- Tuple [Symbol "a", Variant [Symbol "b", Symbol "c"]]

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


allCombinations :: String -> [String]
allCombinations src =
  let expr = parse src in
  let combs = concatMap combinations (maybeToList expr) in
  map show combs

