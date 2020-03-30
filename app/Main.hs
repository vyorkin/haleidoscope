module Main (main) where

import System.IO (stderr)
import qualified Data.Text.Lazy.IO as Text

import LLVM.IRBuilder.Module (buildModuleT)

import Haleidoscope.REPL (repl)
import LLVM.Pretty (ppll)

main :: IO ()
main = do
  buildModuleT "main" repl >>= Text.hPutStrLn stderr . ("\n" <>) . ppll
  main
