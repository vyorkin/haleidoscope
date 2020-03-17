module Haleidoscope.REPL (repl) where

import qualified Haleidoscope.Lexer as Lexer

repl :: IO ()
repl = do
  putStr "> "
  s <- getLine
  putStrLn $ either show show (Lexer.scan s)
  repl
