module Sandbox.Parsing.Eval
  ( Value(..)
  , eval
  ) where

import Sandbox.Parsing.AST

data Value
  = B Bool
  | I Integer
  deriving (Show)

eval :: Term -> Maybe Value
eval = \case
  T -> Just (B True)
  F -> Just (B False)
  Zero -> Just (I 0)
  IsZero x ->
    case eval x of
      Just (I i) -> Just (B (i == 0))
      _ -> Nothing
  Succ x ->
    case eval x of
      Just (I i) -> Just (I (i + 1))
      _ -> Nothing
  Pred x ->
    case eval x of
      Just (I i) | i > 0 -> Just (I (i - 1))
      _ -> Nothing
  IfThen cond t f ->
    case eval cond of
      Just (B True) -> eval t
      Just (B False) -> eval f
      _ -> Nothing
