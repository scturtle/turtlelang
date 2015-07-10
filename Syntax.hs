module Syntax where

data Ty = Nat | Arr Ty Ty
  deriving Show

type Op = String
type SymName = String

data Expr = Num Int
          | Var SymName
          | App Expr Expr
          | Lam Ty SymName Expr
          | BinOp Op Expr Expr
          | Let SymName Expr Expr
          | Ifz Expr Expr Expr
  deriving Show
