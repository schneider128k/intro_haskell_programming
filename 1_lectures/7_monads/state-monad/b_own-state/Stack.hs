module Stack where

import State

newtype Stack a = Stack [a] deriving (Show,Eq)

pop :: State (Stack a) a
pop = State $ \(Stack (x:xs)) -> (x, Stack xs)

push :: a -> State (Stack a) ()
push x = State $ \(Stack xs) -> ((), Stack (x:xs))


stackManip :: State (Stack Int) ()
stackManip =
    pop >>- \a ->
    if even a
      then push a
      else push (2 * a)

test = runState stackManip (Stack [1,2,3])
