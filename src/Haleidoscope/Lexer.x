{
module Haleidoscope.Lexer (scan) where

import Prelude hiding (GT, LT, EQ)
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Numeric (readFloat)

import Haleidoscope.Lexer.Types (Lexeme(..))
}

-- `%wrapper` directive specifies the kind of interface
-- alex should provide to the lexical analyzer it generates

%wrapper "monadUserState"

-- -----------------------------------------------------------------------------
-- Macroses and character sets
-- -----------------------------------------------------------------------------

-- Macro definitions (set of characters or regular expression)
-- Use `$` to prefix a charset macro
-- Use `@` to prefix a regex macro

$whitespace = [\ \t\b]
$digit      = 0-9
$alpha      = [A-Za-z]

@number     = [$digit]+
@float      = @number \. @number
@identifier = $alpha($alpha|_|$digit)*

-- -----------------------------------------------------------------------------
-- Tokens
-- -----------------------------------------------------------------------------

tokens :-

<0> \, { mkT COMMA }
<0> \. { mkT DOT }
<0> \( { mkT LPAREN }
<0> \) { mkT RPAREN }

<0> \+ { mkT PLUS }
<0> \- { mkT MINUS }
<0> \* { mkT TIMES }
<0> \/ { mkT DIV }

<0> \>\= { mkT GE }
<0> \>   { mkT GT }
<0> \<\= { mkT LE }
<0> \<   { mkT LT }

<0> \=   { mkT EQ }
<0> \<\> { mkT NEQ }

<0> "def"    { mkT DEF }
<0> "extern" { mkT EXTERN }

<0> \" { enterNewString `andBegin` state_string }

<state_string> \\n  { addCharToString '\n' }
<state_string> \\t  { addCharToString '\t' }
<state_string> \\\" { addCharToString '\"' }
<state_string> \\\\ { addCharToString '\\' }
<state_string> \\[\ \n\t\f\r\b\v]+\\       ;
<state_string> \"   { leaveString `andBegin` state_initial }
<state_string>  .   { addCurrentToString }
<state_string> \n   { skip }

<0> \n           { skip }
<0> $whitespace+ ;

<0> @float       { getFloat }
<0> @identifier  { getIdentifier }
<0> .            { \_ _ -> lexerError "Illegal character" }

{

-- Data types Alex provides for position info:

-- data AlexPosn
--  = AlexPn !Int -- Absolute character offset
--           !Int -- Line number
--           !Int -- Column number

-- type AlexInput =
--   ( AlexPosn  -- Current position
--   , Char      -- Previous character
--   , [Byte]    -- Rest of the bytes for the current char
--   , String    -- Current input string
--   )

-- -----------------------------------------------------------------------------
-- Token
-- -----------------------------------------------------------------------------

data Token = Token
  AlexPosn       -- Position info
  Lexeme         -- Token lexeme
  (Maybe String) -- Raw matching string

instance Show Token where
  show (Token _ EOF _) = "Token EOF"
  show (Token pos l str) = "Token "
    ++ show l
    ++ " " ++ showPosn pos ++ " "
    ++ maybe "" show str

posn :: Token -> AlexPosn
posn (Token pos _ _) = pos

-- | Makes a new token.
mkT :: Lexeme -> AlexInput -> Int -> Alex Token
mkT l (pos, _, _, str) len = return $ Token pos l (Just (take len str))

isEOF :: Token -> Bool
isEOF (Token _ l _) = l == EOF

-- -----------------------------------------------------------------------------
-- Position info
-- -----------------------------------------------------------------------------

type Pos = Maybe AlexPosn

lineNumber :: Pos -> (Int, Int)
lineNumber Nothing                   = (0, 0)
lineNumber (Just (AlexPn _ lig col)) = (lig, col)

-- | Given a position info returns "line:col".
showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':': show col

-- -----------------------------------------------------------------------------
-- States
-- -----------------------------------------------------------------------------

state_initial :: Int
state_initial = 0

-- -----------------------------------------------------------------------------
-- Actions
-- -----------------------------------------------------------------------------

-- STRING

enterNewString :: Action
enterNewString _ _ = do
  setLexerStringState True
  setLexerStringValue ""
  alexMonadScan

addCharToString :: Char -> Action
addCharToString c _ _ = do
  addCharToLexerStringValue c
  alexMonadScan

addCurrentToString :: Action
addCurrentToString i@(_, _, _, str) len = do
  when (len /= 1) $ error "Invalid call to addCurrentToString"
  addCharToString (head str) i len

-- TODO: Handle special forms like '\nnn' and '\^A'

leaveString :: Action
leaveString (pos, _, _, input) len = do
  s <- getLexerStringValue
  setLexerStringState False
  let str = reverse s
      raw = take len input
  return $ Token pos (STRING str) (Just raw)

-- FLOAT

getFloat :: Action
getFloat (pos, _, _, input) len = do
  when (length r /= 1) $ error "Invalid float"
  let n = fst . head $ r
  return $ Token pos (FLOAT n) (Just s)
  where
    s = take len input
    r = readFloat s

-- IDENT

getIdentifier :: Action
getIdentifier (pos, _, _, input) len = do
  let s = take len input
  return $ Token pos (IDENT s) (Just s)

-- -----------------------------------------------------------------------------
-- The user state monad
-- -----------------------------------------------------------------------------

-- Alex generates the following general state data type:
-- data AlexState = AlexState
--   { alex_pos :: !AlexPosn     -- Position at current input location
--   , alex_inp :: String        -- The current input
--   , alex_chr :: !Char         -- The character before the input
--   , alex_bytes :: [Byte]      -- Rest of the bytes for the current char
--   , alex_scd :: !Int          -- The current startcode
--   , alex_ust :: AlexUserState -- AlexUserState will be defined in the user program
--   }

data AlexUserState = AlexUserState
  { -- Used by the lexer phase
    lexerStringState :: Bool
  , lexerStringValue :: String
    -- Used by the parser phase
  , parserCurrentToken :: Token
  , parserPos :: Pos
  }

getLexerStringState :: Alex Bool
getLexerStringState = Alex $ \s@AlexState { alex_ust = ust} -> Right (s, lexerStringState ust)

setLexerStringState :: Bool -> Alex ()
setLexerStringState ss = Alex $ \s -> Right (s { alex_ust = (alex_ust s) { lexerStringState = ss } }, ())

getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState { alex_ust = ust } -> Right (s, lexerStringValue ust)

setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = Alex $ \s -> Right (s { alex_ust = (alex_ust s) { lexerStringValue = ss } }, ())

addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = Alex $ \s -> Right (s { alex_ust = (alex_ust s) { lexerStringValue = c:lexerStringValue (alex_ust s) } }, ())

getParserCurrentToken :: Alex Token
getParserCurrentToken = Alex $ \s@AlexState { alex_ust = ust } -> Right (s, parserCurrentToken ust)

setParserCurrentToken :: Token -> Alex ()
setParserCurrentToken ss = Alex $ \s -> Right (s { alex_ust = (alex_ust s) { parserCurrentToken = ss } }, ())

getParserPos :: Alex Pos
getParserPos = Alex $ \s@AlexState { alex_ust = ust } -> Right (s, parserPos ust)

setParserPos :: Pos -> Alex ()
setParserPos ss = Alex $ \s -> Right (s { alex_ust = (alex_ust s) { parserPos = ss } }, ())

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { lexerStringState   = False
  , lexerStringValue   = ""
  , parserCurrentToken = Token undefined EOF Nothing
  , parserPos          = Nothing
  }

-- -----------------------------------------------------------------------------
-- Definition needed by Alex

alexEOF :: Alex Token
alexEOF = return $ Token undefined EOF Nothing

-- -----------------------------------------------------------------------------
-- Execution

type Action = AlexInput -> Int -> Alex Token

-- newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

scan :: String -> Either String [Token]
scan s = do
  runAlex s loop
  where
    loop = do
      (tok, err) <- alexComplementError alexMonadScan
      when (isJust err) $ lexerError (fromJust err)
      if isEOF tok
      then do
        ss <- getLexerStringState
        when ss $ alexError "String not closed at end of file"
        return [tok]
      else do
        toks <- loop
        return $ tok : toks

lexerError :: String -> Alex a
lexerError msg = do
  (p, c, _, inp) <- alexGetInput
  let inp1 = filter (/= '\r') (takeWhile (/='\n') inp)
  let inp2 = if length inp1 > 30
             then trim (take 30 inp1)
             else trim inp1
  let disp = if null inp
             then " at end of file"
             else if null inp2
                  then " before end of line"
                  else " on char " ++ show c ++ " before : '" ++ inp2 ++ "'"
  let disp3 = if null msg
              then "Lexer error"
              else trim msg
  alexError (disp3 ++ " at " ++ showPosn p ++ disp)
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- We capture the error message in order to complement it with the file position
alexComplementError :: Alex a -> Alex (a, Maybe String)
alexComplementError (Alex al) =
  Alex (\s -> case al s of
    Right (s', x) -> Right (s', (x, Nothing))
    Left  message -> Right (s, (undefined, Just message)))

}
