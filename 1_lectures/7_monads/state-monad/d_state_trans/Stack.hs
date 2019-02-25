module Stack where

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

test1 :: StateT (Stack String) IO ()
test1 = do
  lift $ putStrLn "Enter first line:"
  line1 <- lift getLine
  push line1
  lift $ putStrLn "Enter second line:"
  line2 <- lift getLine
  push line2
  push $ line1 ++ line2


run t = runStateT t (Stack [])

