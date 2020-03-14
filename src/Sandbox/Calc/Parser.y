{
module Sandbox.Calc.Parser (parse, main) where
-- The code inside curly braces is
-- emitted verbatim into the generated module

import Data.Char (isSpace, isAlpha, isDigit)
import System.IO (hSetBuffering, BufferMode(..), stdout)
import System.IO.Error (tryIOError, isEOFError)

-- In a grammar file, Haskell code is always contained between
-- curly braces to distinguish it from the grammar
}

-- We'll implement a parser for a simple expression syntax,
-- consisting of integers, variables, the operators +, -, *, /,
-- and the form `let var = exp in exp`. Our parser will be a standalone module.

-- Name of the parsing function that Happy will generate.
-- In many cases, this is the only symbol we need to export from the module
%name parse
-- Declares the type of tokens that the parser will accept
%tokentype { Token }
-- Tells Happy the name of a function it
-- should call in the event of a parse error
%error { parseError }

-- Tokens
-- ------

-- The `$$` symbol is a placeholder that represents the value of
-- this token. Normally the value of a token is the token
-- itself, but by using the `$$` symbol you can specify some
-- component of the token object to be the value

%token
  let  { TLet }
  in   { TIn }
  int  { TInt $$ }
  var  { TVar $$ }
  '='  { TEq }
  '+'  { TPlus }
  '-'  { TMinus }
  '*'  { TTimes }
  '/'  { TDiv }
  '('  { TLeftBrace }
  ')'  { TRightBrace }

-- Grammar rules
-- -------------

-- Like yacc, we include `%%` here, for no real reason.

%%

Expr : let var '=' Expr in Expr { Let $2 $4 $6 }
     | Expr1                    { Expr1 $1 }

Expr1 : Expr1 '+' Term { Plus $1 $3 }
      | Expr1 '-' Term { Minus $1 $3 }
      | Term           { Term $1 }

Term : Term '*' Factor { Times $1 $3 }
     | Term '/' Factor { Div $1 $3 }
     | Factor          { Factor $1 }

Factor : int { Int $1 }
       | var { Var $1 }
       | '(' Expr ')' { Brack $2 }

-- Error handler
----------------

{
parseError :: [Token] -> a
parseError _ = error "Parsing error"

-- AST
------

-- Data type that represents a parsed expression

data Expr
  = Let String Expr Expr
  | Expr1 Expr1
  deriving (Show)

data Expr1
  = Plus Expr1 Term
  | Minus Expr1 Term
  | Term Term
  deriving (Show)

data Term
  = Times Term Factor
  | Div Term Factor
  | Factor Factor
  deriving (Show)

data Factor
  = Int Int
  | Var String
  | Brack Expr
  deriving (Show)

data Token
  = TLet
  | TIn
  | TInt Int
  | TVar String
  | TEq
  | TPlus
  | TMinus
  | TTimes
  | TDiv
  | TLeftBrace
  | TRightBrace
  deriving (Show)

-- Lexer
--------

lexer :: String -> [Token]
lexer = \case
  [] -> []
  (c:cs) | isSpace c -> lexer cs
         | isAlpha c -> lexVar (c:cs)
         | isDigit c -> lexNum (c:cs)
  ('=':cs) -> TEq : lexer cs
  ('+':cs) -> TPlus : lexer cs
  ('-':cs) -> TMinus : lexer cs
  ('*':cs) -> TTimes : lexer cs
  ('/':cs) -> TDiv : lexer cs
  ('(':cs) -> TLeftBrace : lexer cs
  (')':cs) -> TRightBrace : lexer cs
  s -> error "Lexing error"
  where
    lexNum :: String -> [Token]
    lexNum cs =
      let (num, rest) = span isDigit cs
      in TInt (read num) : lexer rest

    lexVar :: String -> [Token]
    lexVar cs = case span isAlpha cs of
      ("let", rest) -> TLet : lexer rest
      ("in", rest) -> TIn : lexer rest
      (var, rest) -> TVar var : lexer rest

process :: String -> IO ()
process s =
  let tokens = lexer s in
  let expr = parse tokens in
  print expr

repl :: IO ()
repl = do
  putStr "> "
  line <- tryIOError getLine
  case line of
    Left e | isEOFError e -> putStrLn "Bye!"
    Left e -> ioError e
    Right s | null s -> repl
    Right s -> process s >> repl

main :: IO ()
main = hSetBuffering stdout NoBuffering >> repl
}
