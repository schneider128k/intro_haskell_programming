module PolygonArea where

det :: (Double, Double) -> (Double, Double) -> Double
det (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

-- p1 p2 ... pn
-- det p1 p2 + det p2 p3 + ... + det pn p1

computeArea :: [(Double, Double)] -> Double
computeArea []  = error "empty list"
computeArea [_] = error "singleton list"
computeArea ps  = 0.5 * sum (zipWith det ps (shiftLeft ps))

shiftLeft :: [a] -> [a]
shiftLeft [] = []
shiftLeft (x:xs) = xs ++ [x]





