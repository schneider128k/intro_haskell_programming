inc :: Int -> Int
inc x = x + 1

nothing x = x

equal x y = (x == y)

-- if then else
max' :: (Ord a) => a -> a -> a
max' x y =
  if (x >= y) then x else y

-- guard notation
max'' :: (Ord a) => a -> a -> a
max'' x y
  | (x >= y)  = x
  | otherwise = y

compare' :: (Ord a) => a -> a -> String
compare' x y
  | x == y    = "equal"
  | x >  y    = "bigger"
  | otherwise = "smaller"

describeNum :: Int -> String
describeNum x
  | x == 0    = "zero"
  | x == 1    = "one"
  | otherwise = "big number"

-- pattern matching
describeNum' :: Int -> String
describeNum' 0 = "zero"
describeNum' 1 = "one"
describeNum' _ = "big number"

pair :: Int -> Int -> String
pair 1 _ = "first arg is 1"
pair _ 1 = "second arg is 1"
pair _ _ = "whatever"

-- case statement is syntactic sugar for
-- the implementation using where statement
describeList :: [a] -> String
describeList xs =
  "The list is " ++ case xs of[]  -> "empty"
                              [_] -> "a singelton"
                              _   -> "long" 
-- where statement
describeList' :: [a] -> String
describeList' xs =
  "The list is " ++ what xs
    where what []  = "empty"
          what [_] = "a singelton"
          what _   = "long"

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea  = pi * r ^2
  in  sideArea + 2 * topArea

cylinder' :: (RealFloat a) => a -> a -> a
cylinder' r h =
  sideArea + 2 * topArea
  where sideArea = 2 * pi * r * h
        topArea  = pi * r ^2

dist :: (Num a) => (a, a) -> (a, a) -> a
dist (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)
  
