import Prelude hiding (head,tail,concat,reverse)

head :: [a] -> a
head (x:_)  = x
head []     = error "empty list"

tail :: [a] -> [a]
tail (_:xs) = xs
tail []     = error "empty list"

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge as@(x:xs) bs@(y:ys) =
  if x < y then x:merge xs bs
           else y:merge as ys

-- makeup exam

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

insert :: (Ord a) => a -> [a] -> [a]
insert x []     = [x]
insert x zs@(y:ys) = if x < y then x:zs else y:(insert x ys)
