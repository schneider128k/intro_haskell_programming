-- [1,2,3] syntactic sugar for
-- 1:2:3:[]
--
-- tree is leaning to the right
--   :
--  / \
-- 1   :
--    / \
--   2   :
--      / \
--     3  []

-- foldr
--   r stands for right
--   f is a binary function
--   s is the starting value

--   f                  f                 f
--  / \                / \               / \
-- 1   f        ~~>   1   f         ~~> 1   f 2 (f 3 s)
--    / \                / \
--   2   f              2   (f 3 s)
--      / \
--     3   s

import Prelude hiding (foldr,foldr1)

foldr f s []     = s
foldr f s (x:xs) = f x (foldr f s xs)

foldr1 f [x]    = x
foldr1 f (x:xs) = f x (foldr1 f xs)
foldr1 f []     = error "empty list"

maximum' xs = foldr1 max xs

--   max
--  / \    
-- 1   max
--    / \
--   10  7     (pattern matching first line)
--      / \
--     7   []

-- insert an element into an already sorted list
ins :: (Ord a) => a -> [a] -> [a]
ins x []     = [x]
ins x (y:ys)
  | x <= y    = x:y:ys
  | otherwise = y:(ins x ys)

insertSort :: (Ord a) => [a] -> [a]
insertSort = foldr ins []

insertSort' :: (Ord a) => [a] -> [a]
insertSort' [] = []
insertSort' (x:xs) = ins x (insertSort' xs)

-- quicksort does not use folds
-- but I included it here because we just covered insert sort

quickSort :: (Ord a) => [a] -> [a]
quickSort []     = []
quickSort [x]    = [x]
quickSort (x:xs) =
  left ++ [x] ++ right
  where               -- using list comprehensions
    left  = quickSort [z | z <- xs, z <= x]
    right = quickSort [z | z <- xs, z > x]



