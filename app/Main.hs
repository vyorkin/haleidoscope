module Main (main) where

import System.IO (hSetBuffering, BufferMode(..), stdout)

import Haleidoscope.REPL (repl)
import qualified Haleidoscope.Lexer as Lexer
import Haleidoscope.AST

main :: IO ()
main = return ()
