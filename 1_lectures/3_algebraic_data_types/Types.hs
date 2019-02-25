type Point = (Int, Int)

add :: Point -> Point -> Point
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

type MyString = [Char]

jumble :: MyString -> MyString
jumble = reverse

data TrafficLight = Red | Yellow | Green
  deriving (Show,Eq)

whatToDo :: TrafficLight -> MyString
whatToDo Red    = "stop"
whatToDo Yellow = "accelerate"
whatToDo Green  = "keep going"

data Person = Person String Int deriving Show

yoda = Person "Yoda" 800

