module Haleidoscope.LLVM.Builder
  ( buildExpr
  ) where

import Text.Read (readMaybe)
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as Text
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader (MonadReader(ask))

import qualified LLVM.AST.Type as Type
import LLVM.AST (Operand(ConstantOperand))
import LLVM.IRBuilder.Module (ModuleBuilder)
import LLVM.IRBuilder.Monad (MonadIRBuilder, IRBuilderT)
import LLVM.IRBuilder.Instruction (uitofp)
import LLVM.AST.Constant (Constant(Float))
import LLVM.AST.Float (SomeFloat(Double))

import Haleidoscope.AST
import Haleidoscope.LLVM.Extra (hoist, mostRecentDef)
import LLVM.IRBuilder (fadd, fsub, fmul, fdiv, fcmp)
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate(..))

type Binds = Map String Operand

buildInstr :: MonadIRBuilder m => Op -> Operand -> Operand -> m Operand
buildInstr = \case
  Add -> fadd
  Sub -> fsub
  Mul -> fmul
  Div -> fdiv
  Cmp Ge  -> fcmp OGE
  Cmp Gt  -> fcmp OGT
  Cmp Le  -> fcmp OLE
  Cmp Lt  -> fcmp OLT
  Cmp Eq  -> fcmp OEQ
  Cmp Neq -> fcmp ONE

buildExpr :: Expr -> ReaderT Binds (IRBuilderT ModuleBuilder) Operand
buildExpr = \case
  Num x ->
    pure $ ConstantOperand (Float (Double x))
  Ident s -> do
    binds <- ask
    case binds !? s of
      Just o -> pure o
      Nothing -> error $ "''" <> s <> "' doesn't exist in scope"
  BinOp op a b -> do
    opA <- buildExpr a
    opB <- buildExpr b
    let instr = buildInstr op
    tmp <- instr opA opB
    if isCmp op
    then uitofp tmp Type.double
    else return tmp
