module Haleidoscope.LLVM.Extra
  ( mostRecentDef
  , hoist
  ) where

import Data.Functor.Identity (Identity(..))
import Control.Monad.Trans.State (StateT(..), get)

import LLVM.AST (Definition)
import LLVM.IRBuilder.Module (ModuleBuilderT(..), ModuleBuilder, unModuleBuilderT, builderDefs, liftModuleState)
import LLVM.IRBuilder.Internal.SnocList (getSnocList)

-- Basically the `ModuleBuilderT` is a wrapper around
-- `StateT ModuleBuilderState m a`, where
--
-- data ModuleBuilderState = ModuleBuilderState
--   { builderDefs :: SnocList Definition
--   , builderTypeDefs :: Map Name Type
--   }
--

-- `SnocList` is a `List` where elements are kept in reverse order.
-- Hence the name `snoc`, which is reverse of `cons`. If your're curious -
-- take a look at its definition, as of now it is just 21 lines of code

-- | Gets the most recent definition.
mostRecentDef :: Monad m => ModuleBuilderT m Definition
mostRecentDef = last . getSnocList . builderDefs <$> liftModuleState get

-- type ModuleBuilder = ModuleBuilderT Identity

-- | Hoist a `ModuleBuilder a` (specific to the `Identity` monad)
-- to a `ModuleBuilderT m a` (any `Monad m`)
hoist :: Monad m => ModuleBuilder a -> ModuleBuilderT m a
hoist m = ModuleBuilderT $ StateT $
  return . runIdentity . runStateT (unModuleBuilderT m)
