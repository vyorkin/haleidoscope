module Haleidoscope.LLVM.Builder
  ( buildExpr
  ) where

import Data.String (fromString)
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
import LLVM.IRBuilder.Instruction (uitofp, call)
import LLVM.AST.Constant (Constant(Float, GlobalReference))
import LLVM.AST.Float (SomeFloat(Double))

import Haleidoscope.AST
import Haleidoscope.LLVM.Extra (hoist, mostRecentDef)
import LLVM.IRBuilder (fadd, fsub, fmul, fdiv, fcmp)
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate(OGE, OGT, OLE, OLT, OEQ, ONE))
import LLVM.AST.Type (Type(FunctionType))
import LLVM.AST.AddrSpace (AddrSpace(AddrSpace))

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
    -- Convert an unsigned integer constant to the
    -- corresponding floating-point constant
    then uitofp tmp Type.double
    else return tmp
  Call callee args -> do
    ops <- mapM buildExpr args
    let name = fromString callee
        -- The last argument `False` means that this is not a
        -- variable argument function (http://llvm.org/docs/LangRef.html#int-varargs)
        typ = FunctionType Type.double (replicate (length args) Type.double) False
        --                 ^ result type           ^ args list
        ptrTyp = Type.PointerType typ (AddrSpace 0)
        ref = GlobalReference ptrTyp name
    call (ConstantOperand ref) (zip ops (repeat []))
