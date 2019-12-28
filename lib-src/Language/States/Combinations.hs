module Language.States.Combinations (
  combinations,
  filteredCombinations
) where

import Language.States.Types

import Data.Maybe (mapMaybe)

filteredCombinations :: Expr -> [ExprFilter] -> [Value]
filteredCombinations expr extraFilters =
  let values = combinations expr in
  filterValues values extraFilters

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
filterValues values filters = mapMaybe ((flip applyFilters) filters) values

applyFilters :: Value -> [ExprFilter] -> Maybe Value
applyFilters value []     = Just value
applyFilters value (f:fs) = applyFilter value f >>= (flip applyFilters) fs

applyFilter :: Value -> ExprFilter -> Maybe Value
applyFilter value (EFilter FTRemove pat)    = if matchesPattern value pat then Nothing else Just value
applyFilter value (EFilter FTOnly pat)      = if matchesPattern value pat then Just value else Nothing
applyFilter value (EFilter FTHighlight pat) = if matchesPattern value pat then Just (VHighlighted value) else Just value

matchesPattern :: Value -> Pattern -> Bool
matchesPattern (VTuple vals) (PTuple pats) = (length vals == length pats) && and (zipWith matchesPattern vals pats)
matchesPattern (VVariant i Nothing) (PVariant IPWildcard Nothing)         = True
matchesPattern (VVariant i (Just val)) (PVariant IPWildcard (Just pat))   = matchesPattern val pat
matchesPattern (VVariant i Nothing) (PVariant (IPIdent pi) Nothing)       = (i == pi)
matchesPattern (VVariant i (Just val)) (PVariant (IPIdent pi) (Just pat)) = (i == pi) && matchesPattern val pat
matchesPattern (VTuple _) (PVariant IPWildcard Nothing)                   = True
matchesPattern (VVariant i (Just _)) (PVariant IPWildcard Nothing)        = True
matchesPattern (VHighlighted v) pat                                       = matchesPattern v pat
matchesPattern _ _                                                        = False

