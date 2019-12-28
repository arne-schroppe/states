module Language.States.Types (
  Expr(..),
  VariantOption(..),
  Value(..),
  Pattern(..),
  IdentPattern(..),
  ExprFilter(..),
  FilterType(..),
) where


-- Expressions
data Expr =
    ETuple [Expr]
  | EVariant [VariantOption]
  | EDecl String Expr Expr
  | EVariable String
  | EFiltered Expr [ExprFilter]
  deriving (Show)

data VariantOption =
    EVarOpt String (Maybe Expr)
  deriving (Show)

data ExprFilter = EFilter FilterType Pattern
  deriving (Show)

data FilterType =
    FTRemove
  | FTOnly
  | FTHighlight
  deriving (Show)


-- Values
data Value =
    VTuple [Value]
  | VVariant String (Maybe Value)
  | VHighlighted Value
  deriving (Show)


-- Patterns
data Pattern =
    PTuple [Pattern]
  | PVariant IdentPattern (Maybe Pattern)
  deriving (Show)

data IdentPattern =
    IPWildcard
  | IPIdent String
  deriving (Show)

