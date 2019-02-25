module RPNCalc where

import Control.Monad.State.Lazy

-- state and io combined

newtype Stack a = Stack [a] deriving (Show,Eq)

pop :: StateT (Stack a) IO a
pop = do
  (Stack (x:xs)) <- get
  put (Stack xs)
  return x

push :: a -> StateT (Stack a) IO () 
push x = do
  (Stack xs) <- get
  put (Stack (x:xs))

isOp :: String -> Bool
isOp inp = elem inp ["+","-","*","/"]

getOp :: String -> (Float -> Float -> Float)
getOp "+" = (+)
getOp "-" = (-)
getOp "*" = (*)
getOp "/" = (/)

-- reverse Polish notation calculator

rpnCalc :: StateT (Stack Float) IO ()
rpnCalc = do
  inp <- lift getLine
  if isOp inp
    then do
      b <- pop
      a <- pop
      let r = (getOp inp) a b
      lift $ putStrLn (show r)
      push r
    else push ((read inp)::Float)
  rpnCalc

run = runStateT rpnCalc (Stack [])
