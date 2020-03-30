module Haleidoscope.REPL (repl) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.IO.Error (isEOFError, catchIOError)
import qualified Data.Text.Lazy.IO as Text

import LLVM.IRBuilder.Module (ModuleBuilderT(..))

import Haleidoscope.AST (AST)
import qualified Haleidoscope.Parser as Parser
import qualified Haleidoscope.Lexer as Lexer
import Haleidoscope.LLVM.Builder (buildAST)
import Haleidoscope.LLVM.Extra (hoist, mostRecentDef)
import LLVM.Pretty (ppll)

handle :: String -> ModuleBuilderT IO ()
handle str = case parse str of
  Left err -> liftIO $ hPutStrLn stderr err
  Right ast -> do
    void $ hoist $ buildAST ast
    mostRecentDef >>= liftIO . Text.hPutStrLn stderr . ppll

repl :: ModuleBuilderT IO ()
repl = do
  liftIO $ hPutStr stderr "hal> "
  line <- liftIO $ catchIOError (Just <$> getLine) eofHandler
  maybe (return ()) handle line
  repl

eofHandler :: IOError -> IO (Maybe String)
eofHandler e
  | isEOFError e = return Nothing
  | otherwise = ioError e

parse :: String -> Either String AST
parse str = Lexer.runAlex str Parser.parse
