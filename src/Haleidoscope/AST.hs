module Haleidoscope.AST
  ( Expr(..)
  , Op(..)
  , Rel(..)
  , AST(..)
  ) where

data Expr
  = Num Float
  | Ident String
  | BinOp Op Expr Expr
  | Call String Expr
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

data Prototype = Prototype String [String]
  deriving (Eq, Show)

data AST
  = Function Prototype Expr
  | Extern Prototype
  | TopLevel Expr
  deriving (Eq, Show)
