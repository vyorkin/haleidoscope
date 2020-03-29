module Sandbox.Parsing.Driver (repl, main) where

import qualified Codec.Binary.UTF8.String as String
import System.IO (hSetBuffering, BufferMode(..), stdout)
import System.IO.Error (tryIOError, isEOFError)

import Sandbox.Parsing.AST (Term)
import Sandbox.Parsing.Eval (eval)
import Sandbox.Parsing.Lexer (evalP)
import Sandbox.Parsing.Parser (parse)

output :: Term -> String
output t =
  let r = eval t
  in show t ++ "\n" ++ show r

process :: String -> IO ()
process s = case evalP parse (String.encode s) of
  Left e -> putStrLn e
  Right r -> putStrLn $ output r

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
