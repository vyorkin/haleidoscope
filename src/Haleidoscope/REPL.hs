module Haleidoscope.REPL (repl) where

import Control.Monad.IO.Class (liftIO)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.IO.Error (isEOFError, catchIOError)
import Text.Read (readMaybe)
import qualified Data.Text.Lazy.IO as Text

import LLVM.IRBuilder.Module (ModuleBuilderT(..))

import Haleidoscope.LLVM.Builder (buildAST)
import Haleidoscope.LLVM.Extra (hoist, mostRecentDef)
import LLVM.Pretty (ppll)

handle :: String -> ModuleBuilderT IO ()
handle = error "Implement parser with Happy"
-- handle s = case readMaybe s of
--   Nothing -> liftIO $ hPutStrLn stderr "Couldn't parse"
--   Just ast -> do
--     hoist $ buildAST ast
--     mostRecentDef >>= liftIO . Text.hPutStrLn stderr . ppll

repl :: ModuleBuilderT IO ()
repl = do
  liftIO $ hPutStr stderr "ready>"
  line <- liftIO $ catchIOError (Just <$> getLine) eofHandler
  maybe (return ()) handle line
  repl

eofHandler :: IOError -> IO (Maybe String)
eofHandler e
  | isEOFError e = return Nothing
  | otherwise = ioError e
