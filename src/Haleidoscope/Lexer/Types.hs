module Haleidoscope.Lexer.Types
  ( Lexeme(..)
  ) where

data Lexeme
  = EOF
  | IDENT String
  | DOUBLE Double
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
