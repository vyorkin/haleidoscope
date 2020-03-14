{
module Sandbox.Parsing.Lexer
  ( P
  , Token(..)
  , readToken
  , evalP
  , lexer
  ) where

import Control.Monad.State (StateT, get, put, evalStateT)
import Control.Monad.Except (throwError)
import Data.Word
}

tokens :-
  $white+ ;
  true    {TTrue}
  false   {TFalse}
  0       {TZero}
  succ    {TSucc}
  pred    {TPred}
  if      {TIf}
  then    {TThen}
  else    {TElse}
  iszero  {TIsZero}

{
data Token
  = TTrue
  | TFalse
  | TZero
  | TSucc
  | TPred
  | TIf
  | TThen
  | TElse
  | TIsZero
  | TEOF
  deriving (Show)

-- Next, we define functions that must be provided to Alex's basic interface
-- More info: https://www.haskell.org/alex/doc/html/basic-api.html

type AlexInput = [Word8]

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (b:bs) = Just (b, bs)
alexGetByte []     = Nothing

 -- We don't use patterns with a left-context in our lexical
 -- specification, so we can safely forget about the previous
 -- character in the input stream and return undefined

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

type P a = StateT AlexInput (Either String) a

evalP :: P a -> AlexInput -> Either String a
evalP = evalStateT

-- We have not provided any wrapper specification to Alex, so it
-- provides us with the lowest level interface

-- alexScan
--   :: AlexInput         -- The current input
--   -> Int               -- The "start code"
--   -> AlexReturn action -- The return value

-- data AlexReturn action
--   = AlexEOF
--   | AlexError
--       !AlexInput -- Remaining input
--   | AlexSkip
--       !AlexInput -- Remaining input
--       !Int       -- Token length
--   | AlexToken
--       !AlexInput -- Remaining input
--       !Int       -- Token length
--       action     -- action value

-- The `action` is simply the value of the expression inside
-- {...} on the right-hand-side of the appropriate rule in the
-- Alex file

-- | Function in our `P` monad which produces a new token
readToken :: P Token
readToken = do
  -- Get current AlexInput
  s <- get
  -- Try to scan a single token
  -- (For more info see: 3.2.2.2. Start codes in https://www.haskell.org/alex/doc/html/alex-files.html)
  case alexScan s 0 of
    -- End of output. Return `TEOF`, which
    -- indicates end of input to the parser
    AlexEOF -> return TEOF
    -- Error condition (a valid token could not be recognised).
    -- We use the `throwError` function from `MonadError` class to
    -- raise an exception within our monad
    AlexError _ -> throwError "!Lexical error"
    -- Indicates that some input needs to be skipped over.
    -- `input` - the position in the input from
    -- which scanning should continue (remaining input)
    AlexSkip input _ -> do
      put input -- Store the remaining input in our `StateT`
      readToken -- Try to find a next token
    -- Token has been found
    AlexToken input _ token -> do
      put input    -- Save the remaining input
      return token -- In our case token is simply a value of the `Token` type

lexer :: (Token -> P a) -> P a
lexer cont = readToken >>= cont

}
