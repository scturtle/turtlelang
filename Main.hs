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

import Data.Word
import Data.Maybe
import Data.Traversable (for)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import Debug.Trace (trace)
import Data.List ((\\))

import Syntax
import Codegen

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
constUintSize size = ConstantOperand . C.Int size . fromIntegral

constUint, i32c :: Int -> Operand
constUint = constUintSize uintSize
i32c = constUintSize 32

envVarName :: String
envVarName = "env"

inttoptr :: Operand -> Type -> Codegen Operand
inttoptr a b = instr $ IntToPtr a b []

ptrtoint :: Operand -> Type -> Codegen Operand
ptrtoint a b = instr $ PtrToInt a b []

bitcast :: Operand -> Type -> Codegen Operand
bitcast a b = instr $ BitCast a b []

structType :: [AST.Type] -> AST.Type
structType = AST.StructureType True

lambdaSig :: [AST.Type]
lambdaSig = [uint, uint]

funcType :: AST.Type
funcType = AST.FunctionType uint lambdaSig False

closType :: AST.Type
closType = structType [uint, uint]

genBinOp :: String -> Operand -> Operand -> Codegen Operand
genBinOp opname a b =
  let opd = fromJust $ lookup opname [("+", Add), ("-", Sub), ("*", Mul)]
  in  instr $ opd False False a b []

icmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp cond a b = instr $ ICmp cond a b []

getelementptr :: Operand -> Int -> Codegen Operand
getelementptr addr ix = instr $ GetElementPtr True addr [i32c 0, i32c ix] []

malloc :: Int -> Codegen AST.Operand
malloc bytes = call (externf $ Name "malloc") [i32c $ bytes * uintSizeBytes]

funcOpr :: Type -> Name -> [Type] -> Operand
funcOpr retTy funcname tys = ConstantOperand $
  C.GlobalReference (FunctionType retTy tys False) funcname

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

codegenExpr :: Expr -> Codegen AST.Operand
codegenExpr (Num n) = return . constUint $ n
codegenExpr (Var sym) = getvar sym >>= load
codegenExpr (BinOp op e1 e2) = do
  val1 <- codegenExpr e1
  val2 <- codegenExpr e2
  genBinOp op val1 val2
codegenExpr (Let sym e1 e2) = do
  -- symPtr <- alloca uint
  symRaw <- malloc 1
  symPtr <- bitcast symRaw $ ptr uint
  assign sym symPtr
  -- modify $ \cur -> cur { letVars = sym : letVars cur }
  cur <- get
  val <- trace (show (symtab cur) ++ " " ++ show (letVars cur)) $ codegenExpr e1
  store symPtr val
  codegenExpr e2
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
codegenExpr e@(Lam _ sym expr) = do
  cgst <- get

  let lambdaName = "lambda" ++ show (lambdaCnt cgst)
      freeVars = findFreeVars e \\ letVars cgst
      envType = structType $ replicate (length freeVars) uint

  let loadVars = do
        let envInt = AST.LocalReference uint $ AST.Name envVarName
        envPtr <- inttoptr envInt $ ptr envType
        for (zip [0..] freeVars) $ \(ix, var) -> do
          varPos <- getelementptr envPtr ix
          varPosPtr <- inttoptr varPos $ ptr uint
          varInt <- load varPosPtr
          varPtr <- inttoptr varInt $ ptr uint
          assign var varPtr

  let createFunc = codegenFunction lambdaName lambdaSig loadVars [envVarName, sym] expr

  trace ("freevars: " ++ show freeVars ++ "\n  symtab: " ++ show (symtab cgst)) $
    put $ cgst { extraFuncs = createFunc : extraFuncs cgst
               , lambdaCnt = lambdaCnt cgst + 1 }

  closRaw <- malloc 2
  closInt <- ptrtoint closRaw uint
  closPtr <- inttoptr closInt $ ptr closType
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
  first <- getelementptr closPtr 0
  store first envInt
  second <- getelementptr closPtr 1
  funcInt <- ptrtoint (funcOpr uint (AST.Name lambdaName) lambdaSig) uint
  store second funcInt
  return closInt
codegenExpr (Ifz cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  cur <- get
  condVal <- trace ("ifz: " ++ show (symtab cur)) $ codegenExpr cond
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

-------------------------------------------------------------------------------

prog0 :: Expr
prog0 = Num 42
-- 42

prog1 :: Expr
prog1 = App (App (App (Lam Nat "x" (Lam Nat "y" (Lam Nat "z" (Var "y")))) (Num 1)) (Num 2)) (Num 3)
-- 2

prog2 :: Expr
prog2 = Let "x" (BinOp "+" (Num 1) (Num 1))
                (BinOp "+" (Var "x") (Num 1))
-- 3

prog3 :: Expr
prog3 = Ifz (Num 1) (Num 0) (Num 1)
-- 1

prog :: Expr
prog = Let "fact" (Lam Nat "x"
                    (Ifz (Var "x") (Num 1)
                      (BinOp "*" (Var "x")
                        (App (Var "fact") (BinOp "-" (Var "x") (Num 1))))))
           (App (Var "fact") (Num 5))
-- 24

main :: IO ()
main = putStrLn =<< codegen prog
