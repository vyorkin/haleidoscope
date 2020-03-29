module Haleidoscope.AST
  ( Expr(..)
  , Op(..)
  , Rel(..)
  , Prototype(..)
  , AST(..)
  , isCmp
  ) where

data Expr
  = Num Double
  | Ident String
  | BinOp Op Expr Expr
  | Call String [Expr]
  deriving (Eq, Show)

data Rel
  = Ge
  | Gt
  | Le
  | Lt
  | Eq
  | Neq
  deriving (Eq, Show)

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Cmp Rel
  deriving (Eq, Show)

isCmp :: Op -> Bool
isCmp = \case
  Cmp _ -> True
  _ -> False

data Prototype = Prototype String [String]
  deriving (Eq, Show)

data AST
  = Function Prototype Expr -- ^ Function definition
  | Extern Prototype        -- ^ External function definition
  | TopLevel Expr
  deriving (Eq, Show)
