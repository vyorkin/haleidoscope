{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Sandbox.LLVM.Manual
  ( int
  , defAdd
  , module_
  , toLLVM
  ) where

import qualified Data.ByteString.Char8 as ByteString
import LLVM.AST.Type (Type(IntegerType))
import LLVM.AST (defaultModule, moduleDefinitions, moduleName, Module, Instruction(..), Named((:=), Do), Definition(..), Name(..), Parameter(..), functionDefaults, BasicBlock(..), Operand(..), Terminator(Ret))
import LLVM.AST.Global (Global(..))
import LLVM.Context (withContext)
import LLVM.Module (withModuleFromAST)
import LLVM.Internal.Module (moduleLLVMAssembly)

int :: Type
int = IntegerType 32

defAdd :: Definition
defAdd = GlobalDefinition functionDefaults
  { name = Name "add"
  , parameters =
    ( [ Parameter int (Name "a") []
      , Parameter int (Name "b") []
      ]
    , False)
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body :: BasicBlock
    body = BasicBlock
      (Name "entry")
      [ Name "result" := -- named "virtual" register
          Add False      -- no signed wrap
              False      -- no unsigned wrap
              (LocalReference int (Name "a"))
              (LocalReference int (Name "b"))
              []
      ]
      (Do $ Ret (Just (LocalReference int (Name "result"))) [])

module_ :: Module
module_ = defaultModule
  { moduleName = "sandbox-llvm-manual"
  , moduleDefinitions = [defAdd]
  }

toLLVM :: Module -> IO ()
toLLVM llvmAst = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx llvmAst moduleLLVMAssembly
  putStrLn $ ByteString.unpack llvm
