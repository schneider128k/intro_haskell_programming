module SimpleFunctions where

-- a)
filterFirst :: (a -> Bool) -> [a] -> [a]

-- b)
filterLast :: (a -> Bool) -> [a] -> [a]

-- c)
split :: [a] -> ([a],[a])

-- d)
interleave :: ([a],[a]) -> [a]

-- e)
merge :: (Ord a) => ([a],[a]) -> [a]

-- f)
mergeSort :: (Ord a) => [a] -> [a]


