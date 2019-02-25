data Tree a = Nil | Node a (Tree a) (Tree a)

depth :: Tree a -> Int
depth Nil = 0
depth (Node _ t1 t2) = 1 + max (depth t1) (depth t2)

occurs :: Eq a => a -> Tree a -> Int
occurs _ Nil = 0
occurs y (Node x t1 t2) =
  (if y == x then 1 else 0) + (occurs y t1)+ (occurs y t2)

-- report the element in the order you see them
traverseDF :: Tree a -> [a]
traverseDF Nil = []
traverseDF (Node x t1 t2) = x:(traverseDF t1 ++ traverseDF t2)

t = Node 1 (Node 2 Nil (Node 5 Nil Nil)) (Node 3 Nil (Node 4 Nil Nil))

traverseBF :: Tree a -> [a]
traverseBF t = traverseBF_iter [t] []

traverseBF_iter :: [Tree a] -> [a] -> [a]

traverseBF_iter []       result  = result

traverseBF_iter (Nil:ts) partial =
  traverseBF_iter ts partial
traverseBF_iter ((Node x t1 t2):ts) partial =
  traverseBF_iter (ts ++ [t1] ++ [t2]) (partial ++ [x]) 

-- [(Node x t1 t2):ts] ~~> ts ++ [t1] ++ [t2]
       
pairEntriesWithDepth :: Tree a -> Int -> [(a, Int)]

pairEntriesWithDepth Nil _ = []
pairEntriesWithDepth (Node x left right) depth =
  (x,depth):(pairEntriesWithDepth left (depth+1) ++ pairEntriesWithDepth right (depth+1))

instance (Show a) => Show (Tree a) where
  show tree = init $ unlines [replicate d '.' ++ (show n) | (n,d) <- pairEntriesWithDepth tree 0]
