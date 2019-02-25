findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst _ [] = Nothing
findFirst p (x:xs)
  | p x       = Just x
  | otherwise = findFirst p xs

findLast :: (a -> Bool) -> [a] -> Maybe a
findLast p xs = findFirst p (reverse xs)

--perm :: [a] -> [[a]]
--perm [] = [[]]
--perm (x:xs) = (perm xs)

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) =
  if (x <= y)
    then x:y:ys
    else y:(insert x ys)

insertSort :: (Ord a) => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)

insertSort' :: (Ord a) => [a] -> [a]
insertSort' = foldr insert []   

flatMap :: [[a]] -> [a]
flatMap [] = []
flatMap (x:xs) = x ++ (flatMap xs)

flatMap' :: [[a]] -> [a]
flatMap' xs = foldl (++) [] xs

flatMap'' :: [[a]] -> [a]
flatMap'' xs = foldr (++) [] xs

collect :: [a] -> (a -> [b]) -> [b]
collect xs f = flatMap $ map f xs 

f x = [-x,x]
-- f x = [-x,x]
-- collect f [1,2,3] = [-1,1,-2,2,-3,3]]

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x:(merge xs     (y:ys))
  | otherwise = y:(merge (x:xs) ys)


data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Show,Eq)

flatten :: Tree a -> [a]
flatten Nil = []
flatten (Node x left right) =
  x:(flatten left ++ flatten right)





-- crazy solution
merge' :: (Ord a) => [a] -> [a] -> [a]
merge' xs ys = foldl (\x y -> insert y x) xs ys


