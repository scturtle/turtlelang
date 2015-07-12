-- {-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds #-}

import LLVM.General.AST
import qualified LLVM.General.AST as AST

import qualified LLVM.General.AST.IntegerPredicate as IP
import LLVM.General.AST.Type (ptr)
import LLVM.General.Module
import LLVM.General.Context

import Data.Traversable (for)
import Control.Monad.State
import Control.Monad.Except
import Debug.Trace (trace)

import Syntax
import Codegen

-------------------------------------------------------------------------------

malloc :: Int -> Codegen AST.Operand
malloc bytes = call (externf $ Name "malloc") [i32c $ bytes * uintSizeBytes]

codegenFunction :: SymName -> [AST.Type] -> Codegen a
                -> [SymName] -> Expr -> LLVM ()
codegenFunction funcname argTys prologue args expr = do
  defs <- gets moduleDefinitions
  let nxtLambdaIx = length defs
      cgst' = cgst nxtLambdaIx
  defineFunc uint funcname fnargs (createBlocks cgst')
  -- lift up all lambdas
  sequence_ (extraFuncs cgst')
 where
  fnargs = zip argTys $ map Name args
  cgst nxtLambdaIx = execCodegen funcname nxtLambdaIx $ do
    blk <- addBlock entryBlockName
    setBlock blk
    -- alloca all arguments
    for args $ \a -> do
      var <- alloca uint
      store var (local (Name a))
      assign a var
    prologue
    val <- codegenExpr expr
    ret val

codegenTop :: Expr -> LLVM()
codegenTop = codegenFunction "entryFunc" [] (return ()) []

liftError :: Show b => ExceptT b IO a -> IO a
liftError = runExceptT >=> either (fail . show) return

codegen :: Expr -> IO String
codegen expr = withContext $ \ctx ->
    liftError $ withModuleFromLLVMAssembly ctx (File "prelude.ll") $ \prelude ->
      liftError $ withModuleFromAST ctx ast $ \m -> do
        liftError $ linkModules False m prelude
        moduleLLVMAssembly m
  where mallocDef = external (ptr i8) "malloc" [(i32, UnName 0)]
        ast = runLLVM (emptyModule "entryModule") $ mallocDef >> codegenTop expr

-------------------------------------------------------------------------------

loadEnv :: Type -> [String] -> Codegen [()]
loadEnv envType freeVars = do
  let envInt = AST.LocalReference uint $ AST.Name envVarName
  envPtr <- inttoptr envInt $ ptr envType
  for (zip [0..] freeVars) $ \(ix, var) -> do
      varPos <- getelementptr envPtr ix
      varPosPtr <- inttoptr varPos $ ptr uint
      varInt <- load varPosPtr
      varPtr <- inttoptr varInt $ ptr uint
      assign var varPtr

genEnv :: Type -> [String] -> Codegen Operand
genEnv envType freeVars = do
  envRaw <- malloc $ length freeVars
  envInt <- ptrtoint envRaw uint
  envPtr <- inttoptr envInt $ ptr envType
  for (zip [0..] freeVars) $ \(ix, var) -> do
    fvPtr <- getvar var
    fvVal <- load fvPtr
    varRaw <- malloc 1
    varInt <- ptrtoint varRaw uint
    varPtr <- inttoptr varInt $ ptr uint
    store varPtr fvVal
    varPos <- getelementptr envPtr ix
    store varPos varInt
    modify $ \s -> s { envBindings = (fvPtr, varPtr) : envBindings s }
  return envInt

codegenExpr :: Expr -> Codegen AST.Operand
codegenExpr (Num n) = return . constUint $ n
codegenExpr (Var sym) = getvar sym >>= load

codegenExpr (BinOp op e1 e2) = do
  val1 <- codegenExpr e1
  val2 <- codegenExpr e2
  genBinOp op val1 val2

codegenExpr (App func arg) = do
  closInt <- codegenExpr func
  closPtr <- inttoptr closInt $ ptr closType
  first <- getelementptr closPtr 0
  envInt <- load first
  second <- getelementptr closPtr 1
  funcInt <- load second
  funcPtr <- inttoptr funcInt $ ptr funcType
  operand <- codegenExpr arg
  call funcPtr [envInt, operand]

codegenExpr (Ifz cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  condVal <- codegenExpr cond
  test <- icmp IP.EQ (constUint 0) condVal

  cbr test ifthen ifelse -- Branch based on the condition

  trval <- branch ifthen tr ifexit
  flval <- branch ifelse fl ifexit

  setBlock ifexit
  phi uint [(trval, ifthen), (flval, ifelse)]
 where
  branch block expr ifexit = do
    setBlock block
    val <- codegenExpr expr
    br ifexit
    return val

codegenExpr (Let sym e1 e2) = do
  -- symPtr <- alloca uint
  symRaw <- malloc 1
  symPtr <- bitcast symRaw $ ptr uint
  assign sym symPtr
  val <- codegenExpr e1
  store symPtr val
  -- refill
  bindings <- gets envBindings
  sequence_ [store valPtr val | (fvPtr, valPtr) <- bindings, fvPtr == symPtr]
  codegenExpr e2

codegenExpr e@(Lam sym expr) = do
  cgst <- get

  let lambdaName = "lambda" ++ show (lambdaCnt cgst)
      freeVars = findFreeVars e -- \\ letVars cgst
      envType = structType $ replicate (length freeVars) uint

  let createFunc =
        codegenFunction lambdaName lambdaSig (loadEnv envType freeVars)
                        [envVarName, sym] expr

  put $ cgst { extraFuncs = createFunc : extraFuncs cgst
             , lambdaCnt = lambdaCnt cgst + 1 }

  closRaw <- malloc 2
  closInt <- ptrtoint closRaw uint
  closPtr <- inttoptr closInt $ ptr closType
  envInt <- genEnv envType freeVars
  first <- getelementptr closPtr 0
  store first envInt
  second <- getelementptr closPtr 1
  funcInt <- ptrtoint (funcOpr uint (AST.Name lambdaName) lambdaSig) uint
  store second funcInt
  return closInt

-------------------------------------------------------------------------------

prog0 :: Expr
prog0 = Num 42
-- 42

prog1 :: Expr
prog1 = App (App (App (Lam "x" (Lam "y" (Lam "z" (Var "y")))) (Num 1)) (Num 2)) (Num 3)
-- 2

prog2 :: Expr
prog2 = Let "x" (BinOp "+" (Num 1) (Num 1))
                (BinOp "+" (Var "x") (Num 1))
-- 3

prog3 :: Expr
prog3 = Ifz (Num 1) (Num 0) (Num 1)
-- 1

prog :: Expr
prog = Let "fact" (Lam "x"
                    (Ifz (Var "x") (Num 1)
                      (BinOp "*" (Var "x")
                        (App (Var "fact") (BinOp "-" (Var "x") (Num 1))))))
           (App (Var "fact") (Num 5))
-- 120

main :: IO ()
main = putStrLn =<< codegen prog
