module HuttonRazor where

data Expr =
    Lit Integer
  | Add Expr Expr
  

eval :: Expr -> Integer
eval (Lit int) = int
eval (Add exp1 exp2) = eval exp1 + eval exp2
