-- algebraic data types

data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Eq,Show)

t = Node 1 (Node 2 Nil Nil) (Node 3 Nil Nil)

height :: Tree a -> Int
height Nil = 0
height (Node _ left right) =
  1 + max (height left) (height right)

-- does not work at all
--height' :: Tree a -> Int
--height' tree
--  | tree == Nil = 0
--  | tree == (Node _ left right) ...

func :: Eq a => Tree a -> String
func tree
  | tree == Nil = "empty"
  | otherwise   = "non-empty" 

