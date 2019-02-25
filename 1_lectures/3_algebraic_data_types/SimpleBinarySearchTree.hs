data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Eq,Show)

t = Node 1 (Node 2 Nil Nil) (Node 3 Nil Nil)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Nil = Node x Nil Nil
insert x (Node y left right)
  | x < y     = Node y (insert x left) right
  | x > y     = Node y left (insert x right)
  | otherwise = Node y left right

contains :: (Ord a) => a -> Tree a -> Bool
contains _ Nil = False
contains x (Node y left right)
  | x < y     = contains x left
  | x > y     = contains x right
  | otherwise = True

    
  
