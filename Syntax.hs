module Syntax where

import Data.Set (union)
import qualified Data.Set as S

-- data Ty = Nat | Arr Ty Ty
--   deriving Show

type Op = String
type SymName = String

data Expr = Num Int
          | Var SymName
          | App Expr Expr
          | Lam SymName Expr
          | BinOp Op Expr Expr
          | Let SymName Expr Expr
          | Ifz Expr Expr Expr
  deriving Show

findFreeVars :: Expr -> [SymName]
findFreeVars expr = S.toList $ go expr
  where go (Num _) = S.empty
        go (Var var) = S.singleton var
        go (App e1 e2) = go e1 `union` go e2
        go (Lam var e) = S.delete var $ go e
        go (BinOp _ e1 e2) = go e1 `union` go e2
        go (Let var e1 e2) = go e1 `union` S.delete var (go e2)
        go (Ifz e1 e2 e3) = go e1 `union` go e2 `union` go e3
