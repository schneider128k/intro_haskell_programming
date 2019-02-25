-- define a polymorphic binary tree in Haskell

data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Show,Eq)

-- example of a type constructor with kind * -> *
-- Maybe

-- example of a type constructor with kind * -> * -> *
-- Either

-- data Either a b = Left a | Right b

occursNum :: Eq a => a -> Tree a -> Integer
occursNum _  Nil = 0
occursNum x (Node y left right)
  | x == y    = 1 + (occursNum x left) + (occursNum x right)
  | otherwise =     (occursNum x left) + (occursNum x right)

occurs :: Eq a => a -> Tree a -> Bool    
occurs _ Nil = False
occurs x (Node y left right) = (x == y) || (occurs x left) || (occurs x right)

quicksort :: Ord a => [a] -> [a]
quicksort []  = []
quicksort [x] = [x]
quicksort (x:xs) =
  (quicksort left) ++ [x] ++ (quicksort right)
  where
--    left  = [y | y <- xs, y <= x] 
--    right = [y | y <- xs, y >  x]
    left  = filter (<=x) xs
    right = filter (>x) xs




