-- higher-order functions

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' p (x:xs)
  | p x       = x:(filter' p xs)
  | otherwise = (filter' p xs)

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = (f x):(map' f xs)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _  []         = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _  []    _      = []
zipWith' _  _     []     = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)

pair :: a -> b -> (a,b)
pair x y = (x,y)

-- partially applied function
zip'' :: [a] -> [b] -> [(a,b)]
zip'' = zipWith' pair

-- partially applied function
zip'''' :: [a] -> [b] -> [(a,b)]
zip'''' = zipWith' (,)

-- lambda functions (anonymous functions)
-- \
-- /\
zip''' :: [a] -> [b] -> [(a,b)]
zip''' = zipWith' (\x y -> (x,y))

-- goal: implement zipWith by using zip
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry' f) (zip xs ys)

-- f :: a -> b -> c
-- f x y = ...
--
-- We are not given the two inpus x y
-- but we are given (x,y)
-- we cannot apply f because f expects two input x y

-- uncurry :: (a -> b -> c) -> ((a, b) -> c)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f (x,y) = f x y








