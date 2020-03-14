{
{-# OPTIONS_GHC -Wno-warnings-deprecations -Wno-unused-imports #-}
-- For this project we use Relude (see the relude pkgs on hackage) instead of
-- the default Prelude (from base). Relude defines its own `undefined`, which has warning "attached".
-- Also get rid of unused imports warning from the templates/wrappers.hs.

module Sandbox.Lexing.WordCount (main) where
}

-- Specifies the kind of interface alex should
-- provide to the lexical analyzer it generates.
-- The "basic" wrapper means that alex will
-- provide a function with the following signature:
-- `alexScanTokens :: String -> [Token]`
-- where `Token` is a user-defined type

%wrapper "basic"

-- Macro definitions (set of characters or regular expression)
-- Use `$` to prefix a charset macro
-- Use `@` to prefix a regex macro

$letter = [a-zA-Z]        -- `[]` is a "union" operator
$nonletter = [~$letter\n] -- `~` denotes the complement of the character set `$letter`

-- Rules
-- The beginning of the rules is marked by the `:-` token.
-- The name `tokens` is purely decorative.

-- Each rule is a regular expression followd by action.
-- If more than one rule matches the current input the longest match is preffered.

-- Actions can be either a bare `;` (alex skips the input which matches the rule) or
-- some Haskell code (a function of type `String -> Token`) which is called by
-- alex with the matched input to produce a token

tokens :-
 $nonletter+ ;    -- Sequences of non-letters must be skipped
 $letter+    {id} -- Match longest sequence of one or more letters (`id :: a -> a` returns its input unchanged, type of the token is `String`)

{
main :: IO ()
main = do
  -- Read the line into the string
  s <- getLine
  -- Call alex-generated function, get a list of tokens
  let tokens = alexScanTokens (toString s)
  -- Print each word on a separate line
  mapM_ putStrLn tokens
}
