-- The overall format of the grammar file:

-- <optional module header>
-- <directives>
-- %%
-- <grammar>
-- <optional module trailer>

{
-- The module header contains the module name, exports, and
-- imports. No other code is allowed in the header -- this is
-- because Happy may need to include its own import statements
-- directly after this header

module Haleidoscope.Parser (parse) where

import Prelude hiding (GT, LT, EQ)

import Haleidoscope.AST (AST(..), Expr(..), Op(..), Rel(..), Prototype(..))
import Haleidoscope.Lexer (Alex, Token(..), lexer, showPosn)
import qualified Haleidoscope.Lexer as L
}

-- Name of the parsing function that Happy will generate
%name parse

-- Declares the type of tokens that the parser will accept
%tokentype { Token }

-- The %lexer directive takes two arguments:
-- * <lexer> is the name of the lexical analyser function
-- * <eof> is a token that is to be treated as the end of file.

-- We have a monadic lexer and `Token _ L.EOF _` is the token pattern that
-- denotes end of input. When using a monadic lexer, then parser
-- no longer reads a list of tokens. Instead it calls the
-- lexical analysis function `alexMonadScan` for each new token
-- to be read.

-- Now `parseError :: Token -> Alex a`, which is convenient
-- because the error function probably wants to report the token
-- at which the error has occured (otherwise the lexer would
-- have to store it in the monad)

-- The lexical analysis function has the following type:
-- `lexer :: (Token -> Alex a) -> Alex a`

%lexer { lexer } { T _ L.EOF _ }

-- We need a monadic parser that operates in the `Alex` monad
%monad { Alex }

-- Tells Happy the name of a function it
-- should call in the event of a parse error
%error { parseError }

-- -----------------------------------------------------------------------------
-- Tokens
-- -----------------------------------------------------------------------------

--  %token <name> { <Haskell pattern> }
--         <name> { <Haskell pattern> }
--  ...

-- Here we tell Happy about all the terminal symbols used in the grammar

-- The name of each terminal follows the lexical rules for Happy
-- identifiers given above. There are no lexical differences
-- between terminals and non-terminals in the grammar, so it is
-- recommended that you stick to a convention; for example using
-- upper case letters for terminals and lower case for
-- non-terminals, or vice-versa.

%token
 IDENT    { T _ (L.IDENT $$) _ }
 NUM      { T _ (L.DOUBLE $$) _ }
 -- STRING   { T _ (L.STRING $$) _ }
 ','      { T _ L.COMMA _ }
 '.'      { T _ L.DOT _ }
 '('      { T _ L.LPAREN _ }
 ')'      { T _ L.RPAREN _ }
 '+'      { T _ L.PLUS _ }
 '-'      { T _ L.MINUS _ }
 '*'      { T _ L.TIMES _ }
 '/'      { T _ L.DIV _ }
 '>='     { T _ L.GE _ }
 '>'      { T _ L.GT _ }
 '<='     { T _ L.LE _ }
 '<'      { T _ L.LT _ }
 '=='     { T _ L.EQ _ }
 '!='     { T _ L.NEQ _ }
 'def'    { T _ L.DEF _ }
 'extern' { T _ L.EXTERN  _ }

%left '>=' '>' '<=' '<' '==' '!='
%left '+' '-'
%left '*' '/'

-- -----------------------------------------------------------------------------
-- Grammar rules
-- -----------------------------------------------------------------------------

-- Like in yacc, we include `%%` here, for no real reason.
%%

-- Happy allows you to include type signatures in the grammar
-- file itself, to indicate the type of each production

ast :: { AST }
ast : 'def' prototype expr { Function $2 $3 }
    | 'extern' prototype   { Extern $2 }
    | expr                 { TopLevel $1 }

prototype :: { Prototype }
prototype : IDENT '(' names ')' { Prototype $1 $3 }

expr :: { Expr }
expr : primitive { $1 }
     | binary { $1 }
     | IDENT { Ident $1 }
     | call { $1 }
     | '(' expr ')' { $2 }

call :: { Expr }
call : IDENT '(' args ')' { Call $1 $3 }

args :: { [Expr] }
args : expr { [$1] }
     | args ',' expr { $3 : $1 }

names :: { [String] }
names : IDENT { [$1] }
     | names ',' IDENT { $3 : $1 }

primitive :: { Expr }
primitive : NUM { Num $1 }

binary :: { Expr }
binary : expr '>=' expr { BinOp (Cmp Ge) $1 $3 }
       | expr '>'  expr { BinOp (Cmp Gt) $1 $3 }
       | expr '<=' expr { BinOp (Cmp Le) $1 $3 }
       | expr '<'  expr { BinOp (Cmp Lt) $1 $3 }
       | expr '==' expr { BinOp (Cmp Eq) $1 $3 }
       | expr '!=' expr { BinOp (Cmp Neq) $1 $3 }
       | expr '+'  expr { BinOp Add $1 $3 }
       | expr '-'  expr { BinOp Sub $1 $3 }
       | expr '*'  expr { BinOp Mul $1 $3 }
       | expr '/'  expr { BinOp Div $1 $3 }

{
-- -----------------------------------------------------------------------------
-- Module trailer
-- -----------------------------------------------------------------------------

-- This section is used for placing auxiliary definitions that
-- need to be in the same module as the parser. In small
-- parsers, it often contains a hand-written lexical analyser
-- too. There is no restriction on what can be placed in the
-- module trailer, and any code in there is copied verbatim into
-- the generated parser file.

parseError :: Token -> Alex a
parseError (T pos l raw) = error $ "Parsing error on lexeme "
  ++ show l
  ++ " at " ++ showPosn pos
  ++ maybe "" (\str -> ". Input: " ++ str) raw
}
