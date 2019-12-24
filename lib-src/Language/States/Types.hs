module Language.States.Types (
  Expr(..),
  VariantOption(..),
  Value(..)
) where

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
