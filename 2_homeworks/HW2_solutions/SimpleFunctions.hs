module SimpleFunctions where

-- a)
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst p (x:xs)
  | not (p x)   = xs
      | otherwise   = x:filterFirst p xs

        -- b)
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p = reverse . (filterFirst p) . reverse

filterAtPositions :: (Int -> Bool) -> [a] -> [a]
filterAtPositions p xs = [x | (x,i) <- (zip xs [0..]), p i]

-- c)
split :: [a] -> ([a],[a])
split xs = (filterAtPositions even xs, filterAtPositions odd xs)

-- d)
interleave :: ([a],[a]) -> [a]
interleave (xs,[]) = xs
interleave ([],ys) = ys
interleave ((x:xs),(y:ys)) = x:y:(interleave (xs,ys))

-- e)
merge :: (Ord a) => ([a],[a]) -> [a]
merge (xs,[]) = xs
merge ([],ys) = ys
merge ((x:xs),(y:ys))
  | x < y     = x:(merge (xs,(y:ys)))
      | otherwise = y:(merge ((x:xs),ys))

        -- f)
mergeSort :: (Ord a) => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  =
    merge $ (\(left,right) -> (mergeSort left, mergeSort right)) (split xs)
    
