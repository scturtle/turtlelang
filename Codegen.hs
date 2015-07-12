--------------------------------------------------------------------
-- modified from:
-- http://www.stephendiehl.com/llvm/
-- https://github.com/talw/crisp-compiler/
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Data.Word
import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State

import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.IntegerPredicate as IP

import Syntax (SymName)

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

defineFunc ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
defineFunc retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

external ::  Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

uintSize :: Num a => a
uintSize = 64

i8, i32, uint :: Type
i8 = IntegerType 8
i32 = IntegerType 32
uint = IntegerType uintSize

uintSizeBytes :: Integral a => a
uintSizeBytes  = uintSize `div` 8

constUintSize :: Integral i => Word32 -> i -> Operand
constUintSize size = cons . C.Int size . fromIntegral

constUint, i32c :: Int -> Operand
constUint = constUintSize uintSize
i32c = constUintSize 32

envVarName :: String
envVarName = "__env"

structType :: [AST.Type] -> AST.Type
structType = AST.StructureType True

lambdaSig :: [AST.Type]
lambdaSig = [uint, uint]

funcType :: AST.Type
funcType = AST.FunctionType uint lambdaSig False

closType :: AST.Type
closType = structType [uint, uint]

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(String, Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  , extraFuncs   :: [LLVM ()]                -- Lambdas
  , envBindings  :: [(Operand, Operand)]     -- [(fvPtr, valPtr)]
  , funcName     :: String                   -- CodegenState's function name
  , lambdaCnt    :: Int                      -- Sync
  } {- deriving Show -}

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)
  where sortBlocks = sortBy (compare `on` (idx . snd))

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l s (maketerm t)
  where maketerm (Just x) = x
        maketerm Nothing = error $ "Block has no terminator: " ++ show l

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: SymName -> Int -> CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty [] []

execCodegen :: SymName -> Int -> Codegen a -> CodegenState
execCodegen fname lbdCnt computation = execState (runCodegen computation) $ emptyCodegen fname lbdCnt

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

instr :: Instruction -> Codegen Operand
instr ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref := ins] } )
  return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

named :: String -> Codegen a -> Codegen Operand
named iname m = m >> do
  blk <- current
  let b = Name iname
      (_ := x) = last (stack blk)
  modifyBlock $ blk { stack = init (stack blk) ++ [b := x] }
  return $ local b

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = (var, x) : lcls }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

-------------------------------------------------------------------------------

-- References
local ::  Name -> Operand
local = LocalReference uint

global ::  Name -> C.Constant
global = C.GlobalReference uint

externf :: Name -> Operand
externf = cons . C.GlobalReference uint

funcOpr :: Type -> Name -> [Type] -> Operand
funcOpr retTy funcname tys = cons $
  C.GlobalReference (FunctionType retTy tys False) funcname

-- Arithmetic and Constants

genBinOp :: String -> Operand -> Operand -> Codegen Operand
genBinOp opname a b =
  let opd = fromJust $ lookup opname [("+", Add), ("-", Sub), ("*", Mul)]
  in  instr $ opd False False a b []

icmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp cond a b = instr $ ICmp cond a b []

cons :: C.Constant -> Operand
cons = ConstantOperand

-- Effects
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call False CC.C [] (Right fn) [(x, []) | x <- args] [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

inttoptr :: Operand -> Type -> Codegen Operand
inttoptr a b = instr $ IntToPtr a b []

ptrtoint :: Operand -> Type -> Codegen Operand
ptrtoint a b = instr $ PtrToInt a b []

bitcast :: Operand -> Type -> Codegen Operand
bitcast a b = instr $ BitCast a b []

getelementptr :: Operand -> Int -> Codegen Operand
getelementptr addr ix = instr $ GetElementPtr True addr [i32c 0, i32c ix] []

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty incoming = instr $ Phi ty incoming []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []
