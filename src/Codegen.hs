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

---------------------------
-- Code Generation Setup --
---------------------------

-- All variables will be of a single type, double
double :: Type
double = AST.FLoatingPointType 64 IEEE

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
    , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType  = retty
    , basicBlocks = body
    }

external :: Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
    GlobalDefinition $ functionDefaults {
      name        = Name label
    , linkage     = L.External
    , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType  = retty
    , basicBlocks = []
    }

------------
-- Blocks --
------------

-- With our monad we'll create several functions to manipulate the current block
-- state so that we can push and pop the block "cursor" and append instructions
-- into the current block
entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  
  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  pure (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  pure bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> pure x
    Nothing -> error $ "No such block: " ++ show c

------------------
-- Instructions --
------------------

-- Instructions in LLVM are either numbered sequentially or given explicit
-- variable names (%0, %foo, ...).
-- define i32 @add(i32 %a, i32 %b) {
--   %1 = add i32 %a, %b
--   ret i32 %1
-- }
-- In llvm-hs both these types are represented in a sum type that has the
-- constructors UnName and Name. We'll use numbered expressions and map the
-- numbers to identifiers within our symbol table. With every new instruction,
-- the internal counter will be incremented, for which we add a fresh name supply.
fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  pure $ i + 1

-- Named values within the module have a special type Name.
type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName n ns =
  case Map.lookup n ns of
    Nothing -> (n, Map.insert n 1 ns)
    Just i  -> (n ++ show i, Map.insert n (i + 1) ns)
    
-- We can now work with named LLVM values, so we need to create functions
-- to refer to references of values
local :: Name -> Operand
local = LocalReference double

-- Emits a named value which refers to a toplevel function in our module,
-- or an externally declared function
externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference double

-- We'll implement a simple symbol table as an association list to assign
-- variable names to operand quantities
assign :: String -> Operand -> Codegen ()
assign var x = do
  l <- gets symtab
  modify $ \s -> s { symtab = (var, x) : l }
  
getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

-- Internal function that takes an llvm-hs AST node and push it on the current
-- basic block stack and return the left hand side reference of the instruction
instr :: Instruction -> Codegen Operand
instr ins = do
  n <- fresh 
  blk <- current
  let i = stack blk
      ref = UnName n
  modifyBlock (blk { stack = (ref := ins) : i })
  pure $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator t = do
  blk <- current
  modifyBlock (blk { term = Just t })
  pure t

-- Wrapping the AST nodes for basic arithmetic operations
-- NoFastMathFlags or noFastMathFlags?
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ Fmul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv NoFastMathFlags a b []

-- Basic control flow operations that allow us to direct the control flow between
-- basic blocks and return values
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val)

-- "effect" instructions
-- Take a named function reference and a list of arguments. Evaluate it and
-- invoke it at the current position
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

-- Create a pointer to a stack allocated uninitialized value of the given type
alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

--------------------
-- From AST to IR --
--------------------
