{
module Sandbox.Parsing.Parser (parse) where

import Control.Monad.Error

import Sandbox.Parsing.AST
import Sandbox.Parsing.Lexer
}

-- We need a monadic parser that operates in the `P` monad
%monad { P }
-- We have a monadic lexer and `TEOF` is the
-- token pattern that denotes end of input
%lexer { lexer } { TEOF }
-- Name of the parsing function that Happy will generate.
-- In many cases, this is the only symbol we need to export from the module
%name parse
-- Declares the type of tokens that the parser will accept
%tokentype { Token }
-- Tells Happy the name of a function it
-- should call in the event of a parse error
%error { parseError }

-- Tokens

%token
true   {TTrue}
false  {TFalse}
zero   {TZero}
succ   {TSucc}
pred   {TPred}
if     {TIf}
then   {TThen}
else   {TElse}
iszero {TIsZero}

-- Grammar rules

%%

Term
: true {T}
| false {F}
| zero {Zero}
| succ Term  {Succ $2}
| pred Term {Pred $2}
| if Term then Term else Term {IfThen $2 $4 $6}

-- Error handler

{
parseError :: Token -> P a
parseError _ = throwError "!Parse error"
}
