module Language.States.Types (
  Expr(..),
  EVarOption(..),
  Value(..)
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
