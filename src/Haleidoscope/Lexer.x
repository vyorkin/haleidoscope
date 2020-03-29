{
module Haleidoscope.Lexer
  ( Token(..)
  , Alex
  , scan
  , runAlex
  , alexMonadScan
  , lexer
  , module Haleidoscope.Lexer.Types
  ) where

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
<state_string> .    { addCurrentToString }
<state_string> \n   { skip }
<state_string> \"   { leaveString `andBegin` state_initial }

<0> \n           { skip }
<0> $whitespace+ ;

<0> @float       { getDouble }
<0> @identifier  { getIdentifier }
<0> .            { \_ _ -> lexerError "Illegal character" }

{

-- Data types Alex provides for position and input info:

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

data Token = T
  AlexPosn       -- Position info
  Lexeme         -- Token lexeme
  (Maybe String) -- Raw matching string

instance Show Token where
  show (T _ EOF _) = "Token EOF"
  show (T pos l str) = "Token "
    ++ show l
    ++ " " ++ showPosn pos ++ " "
    ++ maybe "" show str

posn :: Token -> AlexPosn
posn (T pos _ _) = pos

-- | Makes a new token.
mkT :: Lexeme -> AlexInput -> Int -> Alex Token
mkT l (pos, _, _, str) len = return $ T pos l raw
  where
    raw :: Maybe String
    raw = Just $ take len str

isEOF :: Token -> Bool
isEOF (T _ l _) = l == EOF

-- -----------------------------------------------------------------------------
-- Position info
-- -----------------------------------------------------------------------------

type Pos = Maybe AlexPosn

lineNumber :: Pos -> (Int, Int)
lineNumber Nothing = (0, 0)
lineNumber (Just (AlexPn _ line column)) = (line, column)

-- | Given a position info returns "line:col".
showPosn :: AlexPosn -> String
showPosn (AlexPn _ line column) = show line ++ ':': show column

-- -----------------------------------------------------------------------------
-- States
-- -----------------------------------------------------------------------------

state_initial :: Int
state_initial = 0

-- -----------------------------------------------------------------------------
-- Actions
-- -----------------------------------------------------------------------------

-- | Type of the token action.
-- (According to Alex User Guide).
type Action = AlexInput -> Int -> Alex Token

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
  return $ T pos (STRING str) (Just raw)

-- DOUBLE

getDouble :: Action
getDouble (pos, _, _, input) len = do
  when (length res /= 1) $ error "Invalid float"
  let n = fst . head $ res
  return $ T pos (DOUBLE n) (Just str)
  where
    str = take len input
    res = readFloat str

-- IDENT

getIdentifier :: Action
getIdentifier (pos, _, _, input) len = do
  let s = take len input
  return $ T pos (IDENT s) (Just s)

-- -----------------------------------------------------------------------------
-- The user state monad
-- -----------------------------------------------------------------------------

-- Alex generates the following general state data type:

-- data AlexState = AlexState
--   { alex_pos   :: !AlexPosn     -- Position at current input location
--   , alex_inp   :: String        -- The current input
--   , alex_chr   :: !Char         -- The character before the input
--   , alex_bytes :: [Byte]        -- Rest of the bytes for the current char
--   , alex_scd   :: !Int          -- The current startcode
--   , alex_ust   :: AlexUserState -- AlexUserState will be defined in the user program
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
  , parserCurrentToken = T undefined EOF Nothing
  , parserPos          = Nothing
  }

-- -----------------------------------------------------------------------------
-- Definition needed by Alex
-- -----------------------------------------------------------------------------

alexEOF :: Alex Token
alexEOF = return $ T undefined EOF Nothing

-- -----------------------------------------------------------------------------
-- Execution
-- -----------------------------------------------------------------------------

-- Code generated by Alex:

-- newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

-- runAlex :: String -> Alex a -> Either String a
-- runAlex input (Alex f) =
--   let st = AlexState
--     { alex_pos = alexStartPos
--     , alex_inp = input
--     , alex_chr = ’\n’
--     , alex_bytes = []
--     , alex_ust = alexInitUserState
--     , alex_scd = 0
--     } in
--   case f st of
--     Left msg -> Left msg
--     Right (_, a) -> Right a

-- Where `a` is `[Token]` in our case

assertEOFState :: Alex ()
assertEOFState = do
  ss <- getLexerStringState
  when ss $ alexError "String not closed at end of file"

scanToken :: Alex Token
scanToken = do
  (tok, err) <- complementError alexMonadScan
  when (isJust err) $ lexerError (fromJust err)
  when (isEOF tok) $ assertEOFState
  return tok

scan :: String -> Either String [Token]
scan str = do
  runAlex str loop
  where
    loop :: Alex [Token]
    loop = do
      tok <- scanToken
      toks <- loop
      return $ tok : toks

-- | Reports error with the given message.
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

-- | We capture the error message in order to complement it with the file position
complementError :: Alex a -> Alex (a, Maybe String)
complementError (Alex al) =
  Alex (\s -> case al s of
    Right (s', x) -> Right (s', (x, Nothing))
    Left  message -> Right (s, (undefined, Just message)))

-- Code generated by Alex

-- To invoke a scanner under the monad wrapper, use:
-- alexMonadScan :: Alex a

-- | The lexer function to be passed to Happy.
lexer :: (Token -> Alex a) -> Alex a
lexer cont = alexMonadScan >>= cont
}
