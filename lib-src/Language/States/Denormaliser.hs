module Language.States.Denormaliser (
  denormalise
) where

import Language.States.Types


-- This step inlines all variable declarations
denormalise :: Expr -> Either String Expr
denormalise expr = denorm [] expr
  where
    denorm :: [(String, Expr)] -> Expr -> Either String Expr
    denorm decls exp =
      case exp of
        EDecl ident declExpr bodyExpr -> do e <- denorm decls declExpr; denorm ((ident, e):decls) bodyExpr
        EVariable ident               -> findDecl ident decls
        ETuple es                     -> do des <- mapM (denorm decls) es; return $ ETuple des
        EVariant opts                 -> do denormOpts <- mapM (denormVarOpt decls) opts
                                            return $ EVariant denormOpts
        EFiltered expr filters        -> do e <- denorm decls expr; return $ EFiltered e filters

    denormVarOpt :: [(String, Expr)] -> VariantOption -> Either String VariantOption
    denormVarOpt decls (EVarOpt i Nothing)  = Right $ EVarOpt i Nothing
    denormVarOpt decls (EVarOpt i (Just e)) = do de <- denorm decls e; return $ EVarOpt i (Just de)

    findDecl :: String -> [(String, Expr)] -> Either String Expr
    findDecl ident []           = Left $ "Unknown variable '" ++ ident ++ "'"
    findDecl ident ((i,e):rest) = if ident == i then Right e else findDecl ident rest
