module Sandbox.Parsing.AST (Term(..)) where

data Term
  = T
  | F
  | Zero
  | IsZero Term
  | Succ Term
  | Pred Term
  | IfThen Term Term Term
  deriving (Show)
