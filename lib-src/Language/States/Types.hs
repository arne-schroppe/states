module Language.States.Types (
  Expr(..),
  VariantOption(..),
  Value(..),
  Pattern(..),
  IdentPattern(..),
  ExprFilter(..),
  FilterType(..),
  FilteredExpr(..),
) where

data FilteredExpr =
    FExpr Expr [ExprFilter]
    deriving (Show)

data Expr =
    ETuple [Expr]
  | EVariant [VariantOption]
  | EDecl String Expr Expr
  | EVariable String
  deriving (Show)

data VariantOption =
    EVarOpt String (Maybe Expr)
  deriving (Show)

data Value =
    VTuple [Value]
  | VVariant String (Maybe Value)
  deriving (Show)

data ExprFilter = EFilter FilterType Pattern
  deriving (Show)

data FilterType =
    FTRemove
--  | FTOnly
  deriving (Show)

data Pattern =
    PTuple [Pattern]
  | PVariant IdentPattern (Maybe Pattern)
  deriving (Show)

data IdentPattern =
    IPWildcard
  | IPIdent String
  deriving (Show)

