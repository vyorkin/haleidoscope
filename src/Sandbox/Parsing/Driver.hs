module Sandbox.Parsing.Driver (repl, main) where

import qualified Data.Text as Text
import qualified Codec.Binary.UTF8.String as String
import System.IO (hSetBuffering, BufferMode(..))
import System.IO.Error (tryIOError, isEOFError, ioError)

import Sandbox.Parsing.AST (Term)
import Sandbox.Parsing.Eval (eval)
import Sandbox.Parsing.Lexer (evalP)
import Sandbox.Parsing.Parser (parse)

output :: Term -> String
output t =
  let r = eval t
  in show t ++ "\n" ++ show r

encode :: Text -> [Word8]
encode = String.encode . Text.unpack

process :: Text -> IO ()
process s = case evalP parse (encode s) of
  Left e -> putStrLn e
  Right r -> putStrLn $ output r

repl :: IO ()
repl = do
  putStr "> "
  line <- tryIOError getLine
  case line of
    Left e | isEOFError e -> putStrLn "Bye!"
    Left e -> ioError e
    Right s | Text.null s -> repl
    Right s -> process s >> repl

main :: IO ()
main = hSetBuffering stdout NoBuffering >> repl
