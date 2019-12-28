module Language.States.Combinations (
  combinations
) where

import Language.States.Types


combinations :: Expr -> [Value]
combinations expr = case expr of
  EFiltered e filters -> let vs = combinations e in filterValues vs filters
  ETuple es           -> map VTuple $ listCombinations $ map combinations es
  EVariant es         -> concatMap mappingFunc es
    where
      mappingFunc :: VariantOption -> [Value]
      mappingFunc (EVarOpt s Nothing)  = [VVariant s Nothing]
      mappingFunc (EVarOpt s (Just e)) = let vs = combinations e in
                                         map (VVariant s . Just) vs

listCombinations :: [[a]] -> [[a]]
listCombinations (xs:[])  = map pure xs
listCombinations (xs:xss) = [ a:b | a <- xs, b <- listCombinations xss ]
listCombinations []       = []


filterValues :: [Value] -> [ExprFilter] -> [Value]
filterValues values filters = applyRemovalFilter values filters

applyRemovalFilter :: [Value] -> [ExprFilter] -> [Value]
applyRemovalFilter values filters = filter (shouldGoThroughFilter filters) values
  where shouldGoThroughFilter filters value = not $ any (matchesRemovalFilter value) filters

matchesRemovalFilter :: Value -> ExprFilter -> Bool
matchesRemovalFilter val (EFilter FTRemove pat) = matchesPattern val pat
matchesRemovalFilter _ _                        = False

matchesPattern :: Value -> Pattern -> Bool
matchesPattern (VTuple vals) (PTuple pats) = (length vals == length pats) && and (zipWith matchesPattern vals pats)
matchesPattern (VVariant i Nothing) (PVariant IPWildcard Nothing)         = True
matchesPattern (VVariant i (Just val)) (PVariant IPWildcard (Just pat))   = matchesPattern val pat
matchesPattern (VVariant i Nothing) (PVariant (IPIdent pi) Nothing)       = (i == pi)
matchesPattern (VVariant i (Just val)) (PVariant (IPIdent pi) (Just pat)) = (i == pi) && matchesPattern val pat
matchesPattern (VTuple _) (PVariant IPWildcard Nothing)                   = True
matchesPattern (VVariant i (Just _)) (PVariant IPWildcard Nothing)        = True
matchesPattern _ _                                                        = False

