{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds #-}

import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as G

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.IntegerPredicate as IP
import LLVM.General.AST.Type (ptr)
import LLVM.General.Module
import LLVM.General.Context

import Data.Traversable (for)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map

import Syntax
import Codegen

-------------------------------------------------------------------------------

uint :: Type
uint = IntegerType uintSize

uintSize :: Num a => a
uintSize = 64

uintSizeBytes :: Integral a => a
uintSizeBytes  = uintSize `div` 8

entryFuncName :: SymName
entryFuncName = "entryFunc"

constUint :: Integral i => i -> Operand
constUint = ConstantOperand . C.Int uintSize . fromIntegral

funcOpr :: Type -> Name -> [Type] -> Operand
funcOpr retTy funcname tys = ConstantOperand $
  C.GlobalReference (FunctionType retTy tys False) funcname

codegenFunction :: SymName -> [AST.Type] -> Codegen a
                -> [SymName] -> Expr -> LLVM ()
codegenFunction funcname argTys prologue args expr = do
  defineFunc uint funcname fnargs (createBlocks cgst)
  -- lift up all lambdas
  sequence_ (extraFuncs cgst)
 where
  fnargs = zip argTys $ map AST.Name args
  cgst = execCodegen funcname $ do
    blk <- addBlock entryBlockName
    setBlock blk
    -- alloca all arguments
    forM_ args $ \a -> do
      var <- alloca uint
      store var (local (AST.Name a))
      assign a var
    prologue
    val <- codegenExpr expr
    ret val

codegenTop :: Expr -> LLVM()
codegenTop {- expr -} = codegenFunction entryFuncName [] (return ()) [] {- expr -}

liftError :: Show b => ExceptT b IO a -> IO a
liftError = runExceptT >=> either (fail . show) return

codegen :: Expr -> IO String
codegen expr = withContext $ \ctx ->
    liftError $ withModuleFromLLVMAssembly ctx (File "prelude.ll") $ \prelude ->
      liftError $ withModuleFromAST ctx ast $ \m -> do
        liftError $ linkModules False m prelude
        moduleLLVMAssembly m
  where ast = runLLVM (emptyModule "init") $ codegenTop expr

-------------------------------------------------------------------------------

codegenExpr :: Expr -> Codegen AST.Operand
codegenExpr (Num n) = return . constUint $ n
-- TODO

-------------------------------------------------------------------------------

prog0 :: Expr
prog0 = Num 42

prog :: Expr
prog = Let "fact" (Lam Nat "x"
                    (Ifz (Var "x") (Num 1)
                      (BinOp "*" (Var "x")
                        (App (Var "fact") (BinOp "-" (Var "x") (Num 1))))))
           (App (Var "fact") (Num 5))

main :: IO ()
main = putStrLn =<< codegen prog0
