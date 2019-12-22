module Lib
( allCombinations
) where

data Expr =
    ETuple [Expr]
  | EVariant [EVarOption]
  deriving (Show)

data EVarOption =
    EVarOpt String (Maybe Expr)
  deriving (Show)

data Value =
    VTuple [Value]
  | VVariant String (Maybe Value)
  deriving (Show)

parse :: String -> Expr
parse src = ETuple[ EVariant [EVarOpt "a" Nothing, EVarOpt "x" Nothing], EVariant [EVarOpt "b" (Just (EVariant [EVarOpt "c" Nothing, EVarOpt "d" Nothing]))]] -- Tuple [Symbol "a", Variant [Symbol "b", Symbol "c"]]

combinations :: Expr -> [Value]
combinations expr = case expr of
  ETuple es -> map VTuple $ listCombinations $ map combinations es
  EVariant es -> flatmap mappingFunc es
    where
      mappingFunc :: EVarOption -> [Value]
      mappingFunc (EVarOpt s Nothing)  = [VVariant s Nothing]
      mappingFunc (EVarOpt s (Just e)) = let vs = combinations e in
                                         map (\x -> VVariant s $ Just x) vs

flatmap :: (a -> [b]) -> [a] -> [b]
flatmap _ [] = []
flatmap f (x:xs) = f x ++ flatmap f xs


listCombinations :: [[a]] -> [[a]]
listCombinations (xs:[])  = map (\x -> [x]) xs
listCombinations (xs:xss) = [ [a] ++ b | a <- xs, b <- listCombinations xss ]
listCombinations []       = []


allCombinations :: String -> [String]
allCombinations src =
  let expr = parse src in
  let combs = combinations expr in
  map show combs
