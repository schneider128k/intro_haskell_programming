-- abstract data type stack
-- with operations
--   pop and push

-- no state

newtype Stack a = Stack [a]
  deriving (Show,Eq)

           --     initial     elem  stack
--     stack         |   after the pop operation
--       |           |     |
--       |           |     |
pop :: Stack a  ->  (a, Stack a)
pop (Stack (x:xs)) = (x, Stack xs)

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

stackManip :: Stack Int -> (Int, Stack Int)
stackManip s0 =
  let (_ ,s1) = pop s0
      (x2,s2) = pop s1
  in (x2,s2)
