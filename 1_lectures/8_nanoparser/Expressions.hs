module Expressions where

data Expr =   Add Expr Expr
            | Mul Expr Expr
            | Sub Expr Expr
            | Lit Int
              deriving Show

eval :: Expr -> Int
eval expr = case expr of
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Sub a b -> eval a - eval b
  Lit n   -> n
  
                      
