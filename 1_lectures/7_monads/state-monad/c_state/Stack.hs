module Stack where

import Control.Monad.State

newtype Stack a = Stack [a] deriving (Show,Eq)

pop :: State (Stack a) a
pop = do
  (Stack (x:xs)) <- get
  put (Stack xs)
  return x

push :: a -> State (Stack a) ()
push x = do
  (Stack xs) <- get
  put (Stack (x:xs))

stackManip :: State (Stack Int) ()
stackManip = do
  a <- pop
  if even a
    then push a
    else push (2 * a)

test = runState stackManip (Stack [1,2,3])
