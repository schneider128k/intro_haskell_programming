module ExpressionsParser where

import Control.Monad

import NanoParser

import Expressions

-- grammar for a simple calculator in Backus-Naur-Form (BNF)

-- number = [ "-" ] digit { digit }.
-- digit  = "0" | "1" | ... | "8" | "9".
-- expr   = term { addop term }.
-- term   = factor { mulop factor }.
-- factor = "(" expr ")" | number.
-- addop  = "+" | "-".
-- mulop  = "*".

int :: Parser Expr
int = do
  n <- number
  return (Lit n)

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = factor `chainl1` mulop

factor :: Parser Expr
factor =
      int `option` parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) `option` (infixOp "-" Sub)

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul

run :: String -> Expr
run = runParser expr

main :: IO ()
main = do
    putStr "> "
    a <- getLine
    if a /= "end"
    then do
      print $ "AST : " ++ (show $ run a)
      print $ "eval: " ++ (show $ eval $ run a)
      main
    else
      print "end"

