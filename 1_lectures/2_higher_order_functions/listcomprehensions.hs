filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap p f xs = [f x | x <- xs, p x]

square x = x * x

