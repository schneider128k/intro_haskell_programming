import Prelude hiding (dropWhile, reverse)

-- dropWhile even [2,4,6,9,2,10] ~~> [9,2,10] 

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p ys@(x:xs)
  | p x       = dropWhile p xs
  | otherwise = ys

-- dropWhile does not have pending computation


-- filterAtMostN 2 even [1,2,3,4,5,6,7] ~~> [2,4,5,6,7] 
filterAtMostN :: Int -> (a -> Bool) -> [a] -> [a]
filterAtMostN _ _ [] = []
filterAtMostN 0 _ xs = xs
filterAtMostN n p (x:xs)
  | p x       = x:filterAtMostN n     p xs
  | otherwise =   filterAtMostN (n-1) p xs

-- filterAtMostN does have pending computation

-- reverse [1,2,3,4] ~~> [4,3,2,1]

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = (reverse xs) ++ [x]

-- reverse has pending computation

reverse' :: [a] -> [a] 
reverse' xs = reverse_iter xs []

reverse_iter :: [a] -> [a] -> [a]
reverse_iter []     result  = result
reverse_iter (x:xs) partial =
  reverse_iter xs (x:partial)

-- occursNum 'l' "Haskell" ~~> 2

occursNum :: (Eq a) => a -> [a] -> Int
occursNum _ [] = 0
occursNum x (y:ys)
  | x == y    = 1 + occursNum x ys
  | otherwise =     occursNum x ys

-- recursive data type

data List a = Empty | Cons a (List a)
  deriving (Show, Eq)

headL :: List a -> a
headL Empty      = error "empty"
headL (Cons x _) = x

tailL :: List a -> List a
tailL Empty      = error "empty"
tailL (Cons _ t) = t

transform :: [a] -> List a
transform []     = Empty
transform (x:xs) = Cons x (transform xs)


