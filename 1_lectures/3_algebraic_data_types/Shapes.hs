data Shape = Circle Float | Rectangle Float Float | Square Float

instance Show Shape where
  show (Circle r)      = "Circle of radius " ++ (show r)
  show (Rectangle h w) = "Rectangle of height " ++ (show h) ++ ", width " ++ (show w)
  show (Square s)      = "Square of side " ++ (show s)

area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle h w) = h * w
area (Square s)      = s * s
