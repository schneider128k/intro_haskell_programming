module Expressions where

import Store
import Data.Maybe

type Var = Char

data Expr =   Lit Integer
            | Var Var
            | Op Ops Expr Expr
              deriving (Show,Eq)
                       
data Ops = Add | Sub | Mul | Div
              deriving (Show,Eq)

eval :: Expr -> (Store Var Integer) -> Integer
eval (Lit x) _ = x
eval (Var var) context
  | isNothing val = error "variable undefined"
  | otherwise     = fromJust val
    where
      val = value context var

eval (Op Add e1 e2) context =
  eval e1 context + eval e2 context
eval (Op Sub e1 e2) context =
  eval e1 context - eval e2 context
eval (Op Mul e1 e2) context =
  eval e1 context * eval e2 context
eval (Op Div e1 e2) context =
  eval e1 context `div` eval e2 context

-- String           Expr
-- "10.0"      ~~>  Lit 10.0
-- "10.0 + x"  ~~>  Op Add (Lit 10.0) (Var 'x')
