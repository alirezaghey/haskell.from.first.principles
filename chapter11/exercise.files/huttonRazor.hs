module HuttonRazor where

data Expr =
    Lit Integer
  | Add Expr Expr
  

-- evaluates any number of nested Expr to an Integer
eval :: Expr -> Integer
eval (Lit int) = int
eval (Add exp1 exp2) = eval exp1 + eval exp2


-- prints the mathematical expression that Expr represents
printExpr :: Expr -> String 
printExpr (Lit int) = show int
printExpr (Add expr1 expr2) = printExpr expr1 ++ " + " ++ printExpr expr2