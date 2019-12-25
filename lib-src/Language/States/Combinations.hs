module Language.States.Combinations (
  filteredCombinations
) where

import Language.States.Types

filteredCombinations :: FilteredExpr -> [Value]
filteredCombinations (FExpr expr filters) =
  let values = combinations expr in
  let filteredValues = filterValues values filters in
  filteredValues

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

filterValues :: [Value] -> [ExprFilter] -> [Value]
filterValues values filters = filter (not . (flip matchesFilters) filters) values

matchesFilters :: Value -> [ExprFilter] -> Bool
matchesFilters val filters = any (matchesFilter val) filters

matchesFilter :: Value -> ExprFilter -> Bool
matchesFilter val (EFilter _ pat) = matchesPattern val pat

matchesPattern :: Value -> Pattern -> Bool
matchesPattern (VTuple vals) (PTuple pats) = (length vals == length pats) && and (zipWith matchesPattern vals pats)
matchesPattern (VVariant i Nothing) (PVariant IPWildcard Nothing)         = True
matchesPattern (VVariant i (Just val)) (PVariant IPWildcard (Just pat))   = matchesPattern val pat
matchesPattern (VVariant i Nothing) (PVariant (IPIdent pi) Nothing)       = (i == pi)
matchesPattern (VVariant i (Just val)) (PVariant (IPIdent pi) (Just pat)) = (i == pi) && matchesPattern val pat
matchesPattern (VTuple _) (PVariant IPWildcard Nothing)                   = True
matchesPattern (VVariant i (Just _)) (PVariant IPWildcard Nothing)        = True
matchesPattern _ _                                                        = False

