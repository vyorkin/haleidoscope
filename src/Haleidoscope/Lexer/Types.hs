module Haleidoscope.Lexer.Types
  ( Lexeme(..)
  ) where

data Lexeme
  = EOF
  | IDENT String
  | FLOAT Float
  | STRING String
  | COMMA
  | DOT
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | GE
  | GT
  | LE
  | LT
  | EQ
  | NEQ
  | DEF
  | EXTERN
  deriving (Eq, Show)
