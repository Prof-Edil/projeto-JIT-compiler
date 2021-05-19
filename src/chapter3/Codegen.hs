{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This module will hold the pure code generation logic to drive building the 
-- AST
module Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST

import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Type Level
-------------------------------------------------------------------------------
-- All variables will be of a single type, double
double :: Type
double = FLoatingPointType 64 IEEE

type SymbolTable = [(String, Operand)]

-- This record type the internal state of our code generator as we walk the AST.
-- This is for the toplevel module code generation.
data CodegenState
    = CodegenState {
      currentBlock :: Name                     -- Name of the active block to append to
    , blocks       :: Map.Map Name BlockState  -- Blocks for function
    , symtab       :: SymbolTable              -- Function scope symbol table
    , blockCount   :: Int                      -- Count of basic blocks
    , count        :: Word                     -- Count of unnamed instructions
    , names        :: Names                    -- Name Supply
    } deriving Show

-- This record is for basic blocks inside of function definitions.
data BlockState
    = BlockState {
      idx   :: Int                             -- Block index
    , stack :: [Named Instruction]             -- Stack of instructions
    , term  :: Maybe (Named Terminator)        -- Block terminator
    } deriving Show

-- We'll hold the state of the code generator inside of Codegen State monad.
-- The Codegen monad contains a map of block names to their BlockState representation.
newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState)

-- LLVM State monad which will hold all code for the LLVM module and upon evaluation
-- will emit an llvm-hs Module containing the AST. We'll append to the list of definitions
-- in the AST.Module field moduleDefinitions.
newtype LLVM a = LLVM (State AST.Module a)
    deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
    defs <- gets moduleDefinitions
    modify $ \s -> s { moduleDefinitions = defs ++ [d] }

-- Toplevel definitions: local functions and external function declarations.
define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
    GlobalDefinition $ functionDefaults {
      name        = Name label
    , parameters  = ([Parameter ty n, [] | (ty, nm) <- argtys], False)
    , returnType  = retty
    , basicBlocks = body
    }

external :: Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
    GlobalDefinition $ functionDefaults {
      name       = Name label
    , linkage    = L.External
    , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , 
    }
    }
