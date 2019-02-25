-- a

data Shapes = Rectangle Float Float | Circle Float

area :: Shapes -> Float
area (Rectangle w h) = w * h
area (Circle r)      = pi * r * r

data Tree a = Nil | Node a (Tree a) (Tree a)
                deriving (Show,Eq)
-- b

traverse :: Tree a -> [a]
traverse Nil                 = []
traverse (Node x left right) = [x] ++ (dfs left) ++ (dfs right)

-- c

occurs :: Eq a => Tree a -> a -> Bool
occurs Nil _                 = False
occurs (Node x left right) y = (x == y) || occurs left y || occurs right y


