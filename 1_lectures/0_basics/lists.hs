head' :: [a] -> a
head' []     = error "headless"
head' (x:_) = x

-- case statement (compare to guards)
head'' :: [a] -> a
head'' xs = case xs of []    -> error "headless"
                       (x:_) -> x

tail' :: [a] -> [a]
tail' []     = error "empty"
tail' (_:xs) = xs

-- outputs True is list has more than 2 elements
isLong :: [a] -> Bool
isLong []     = False
isLong [_]    = False
isLong [_,_]  = False
isLong _      = True

len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + len xs

-- tail recursion
-- pending computation

-- tail recursion
len' :: [a] -> Int
len' xs = len_iter xs 0

len_iter :: [a] -> Int -> Int
len_iter []     n = n
-- there is no pending computation
len_iter (_:xs) n = len_iter xs (n+1)

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x] 

reverse'' :: [a] -> [a]
reverse'' xs = reverse_iter xs []

reverse_iter :: [a] -> [a] -> [a]
reverse_iter []     ys = ys
reverse_iter (x:xs) ys = reverse_iter xs (x:ys)

-- [1,2,3] syntactic sugar for
-- 1:2:3:[]
--
--

-- infix vs prefix

-- 10 + 20 + 30
-- plus (plus 10 20) 30

