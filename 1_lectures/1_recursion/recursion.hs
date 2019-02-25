-- pending computation
maximum' :: (Ord a) => [a] -> a
maximum' []     = error "empty!!!"
maximum' [x]    = x
maximum' (x:xs) = max x (maximum' xs)

maximum'' :: (Ord a) => [a] -> a
maximum'' []     = error "empty!!!"
maximum'' (x:xs) = maximum_iter xs x

maximum_iter :: (Ord a) => [a] -> a -> a
maximum_iter []     m = m
maximum_iter (x:xs) m = maximum_iter xs (max x m)

-- pattern matching
maximum''' :: (Ord a) => [a] -> a
maximum''' []       = error "empty!!!"
maximum''' [x]      = x
maximum''' (x:y:zs) =
  maximum' ((max x y):zs) 
