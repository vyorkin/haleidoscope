module Haleidoscope.Parser.Types
  ( P
  , ParseResult(..)
  , thenP
  , returnP
  ) where

data ParseResult a
  = ParseOk a
  | ParseFail String

-- | Our monad `P` serves three purposes:
-- | * it passes the input string around
-- | * it passes the current line number around
-- | * it deals with success/failure
type P a = String -> Int -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l ->
  case m s l of
    ParseOk a -> k a s l
    ParseFail s -> ParseFail s

returnP :: a -> P a
returnP a = const . const $ ParseOk a
