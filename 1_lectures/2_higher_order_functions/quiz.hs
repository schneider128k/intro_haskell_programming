
-- split [1,2,3,4,5]     ~~> ([1,3,5],[2,4])

-- merge ([1,3,5],[2,4]) ~~> [1,2,3,4,5]

split :: [a] -> ([a],[a])
split []       = ([],[])
split [x]      = ([x],[])
split (x:y:zs) = (x:left, y:right)
  where
    left  = fst (split zs)
    right = snd (split zs)

split' :: [a] -> ([a],[a])
split' []       = ([],[])
split' [x]      = ([x],[])
split' (x:y:zs) = prepend x y (split zs)

prepend :: a -> a -> ([a],[a]) -> ([a],[a])
prepend x y (xs,ys) = (x:xs,y:ys)

merge :: ([a],[a]) -> [a]
merge ([], right) = right
merge (left, [])  = left
merge (x:xs,y:ys) = x:y:merge (xs,ys)
