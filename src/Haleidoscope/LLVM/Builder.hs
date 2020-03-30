module Haleidoscope.LLVM.Builder
  ( buildAST
  , buildExpr
  ) where

import Data.String (fromString)
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Reader (MonadReader(ask))

import qualified LLVM.AST.Type as Type
import LLVM.AST (Operand(ConstantOperand))
import LLVM.AST.Attribute (ParameterAttribute)
import LLVM.IRBuilder.Module (ModuleBuilder)
import LLVM.IRBuilder.Monad (MonadIRBuilder, IRBuilderT)
import LLVM.IRBuilder.Instruction (uitofp, call, ret)
import LLVM.AST.Constant (Constant(Float, GlobalReference))
import LLVM.AST.Float (SomeFloat(Double))

import Haleidoscope.AST
import LLVM.IRBuilder (ParameterName, fadd, fsub, fmul, fdiv, fcmp, function, extern)
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate(OGE, OGT, OLE, OLT, OEQ, ONE))
import LLVM.AST.Type (Type(FunctionType))
import LLVM.AST.AddrSpace (AddrSpace(AddrSpace))
import LLVM.AST.Name (Name)

-- lvm-hs provides a monadic way of building up modules and
-- functions, with ModuleBuilder and IRBuilder respectively.
-- To generate our code we will traverse our AST inside
-- these monads, spitting out LLVM IR as we go along

-- | Map of operand bindings.
-- An 'Operand' is roughly that which is an argument to an 'LLVM.AST.Instruction.Instruction'
type Binds = Map String Operand

-- | Reads operand bindings, emits IR.
type ExprBuilder a = ReaderT Binds (IRBuilderT ModuleBuilder) a

-- data Operand
--   = LocalReference Type Name
--   | ConstantOperand Constant
--   | MetadataOperand Metadata
--

-- `IRBuilderT` provides a uniform API for creating instructions and inserting them
-- into a basic block: either at the end of a `BasicBlock`, or at a specific
-- location in a block

-- newtype IRBuilderT m a =
--   IRBuilderT { unIRBuilderT :: StateT IRBuilderState m a }
--
-- where
--
-- data IRBuilderState = IRBuilderState
--   { builderSupply :: !Word
--   , builderUsedNames :: !(Map ShortByteString Word)
--   , builderNameSuggestion :: !(Maybe ShortByteString)
--   , builderBlocks :: SnocList BasicBlock  -- reversed list of the basic blocks
--   , builderBlock :: !(Maybe PartialBlock) -- (current) partially constructed block
--   }

-- type ModuleBuilder = ModuleBuilderT Identity
--
-- where
--
-- newtype ModuleBuilderT m a =
--   ModuleBuilderT { unModuleBuilderT :: StateT ModuleBuilderState m a }
-- and
--
-- data ModuleBuilderState = ModuleBuilderState
--   { builderDefs :: SnocList Definition
--   , builderTypeDefs :: Map Name Type
--   }
--
-- data Definition
--   = GlobalDefinition Global
--   | TypeDefinition Name (Maybe Type)
--   | ... metadata-related constructors ......................
--   | ... and other things we're not interested in for now ...
--
-- data Global
--   = GlobalVariable { ... }
--   | GlobalAlias { ... }
--   | Function { ... }

buildAST :: AST -> ModuleBuilder Operand
buildAST = \case
  Function (Prototype nameStr argsStrs) bodyExpr -> do
    let name :: Name
        name = fromString nameStr
        args :: [(Type, ParameterName)]
        args = zip (repeat Type.double) (fromString <$> argsStrs)
        -- The LLVM instruction set consist of several different classifications
        -- of instructions: terminator instructions, binary instructions,
        -- bitwise instructions, memory instructions and other instructions
        -- More info: https://llvm.org/docs/LangRef.html#ret-instruction reference
        -- Every basic block in a program ends with a "Terminator"
        -- (control flow) instruction, which indicates which block
        -- should be executed after the current block is finished.
        -- Here we use `ret` as such "Terminator" to return
        -- control flow from a function back to the caller
        body :: ExprBuilder ()
        body = buildExpr bodyExpr >>= ret
    -- Define and emit a (non-variadic) function definition
    function name args Type.double $ \ops -> do
      let binds :: Binds
          binds = Map.fromList (zip argsStrs ops)
      runReaderT body binds
  Extern (Prototype nameStr argsStrs) -> do
    let name = fromString nameStr
        args = replicate (length argsStrs) Type.double
    -- Define external function
    extern name args Type.double
  TopLevel x -> do
    let expr = buildExpr x >>= ret
        body = const $ runReaderT expr mempty
    function "__anon_expr" [] Type.double body

buildExpr :: Expr -> ExprBuilder Operand
buildExpr = \case
  Num x ->
    -- Emit a constant value
    pure $ ConstantOperand (Float (Double x))
  Ident s -> do
    -- Lookup the corresponding `Operand` emit it
    binds <- ask
    case binds !? s of
      Just o -> pure o
      Nothing -> error $ "'" <> s <> "' doesn't exist in scope"
  BinOp op a b -> do
    -- Evaluate (emit) expressions for both operands
    opA <- buildExpr a
    opB <- buildExpr b
    -- Build the instruction constructor/emitter function
    let instr = buildInstr op
    -- Emit the instruction and get back the result
    tmp <- instr opA opB
    -- If it is a comparison instruction
    -- then we know that it returns an unsigned integer
    if isCmp op
    -- Convert an unsigned integer constant to the
    -- corresponding floating-point constant
    then uitofp tmp Type.double
    -- Otherwhise return the result as is
    else return tmp
  Call callee args -> do
    -- Emit call expression
    ops <- mapM buildExpr args
    let name :: Name
        name = fromString callee
        argsTypes :: [Type]
        argsTypes = replicate (length args) Type.double
        -- The last argument `False` means that this is not a
        -- variable argument function (http://llvm.org/docs/LangRef.html#int-varargs)
        typ :: Type
        typ = FunctionType Type.double argsTypes False
        --                   ^ result type   ^ args list
        ptrTyp :: Type
        ptrTyp = Type.PointerType typ (AddrSpace 0)
        -- Reference to a function we want to call
        ref :: Constant
        ref = GlobalReference ptrTyp name
        -- Function arguments (wihtout attributes)
        params :: [(Operand, [ParameterAttribute])]
        params = zip ops (repeat [])
    call (ConstantOperand ref) params

-- | Builds an instruction constructor function.
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
