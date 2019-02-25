module Store
  ( Store,
    initial, -- Store a b
    value,   -- Store a b -> a -> Perhaps b
    update,  -- Store a b -> a -> b -> Store a b
  ) where

newtype Store a b = Store (a -> Perhaps b)

-- Perhaps is our implementation of Maybe (built-in)
data Perhaps b = Undefined | Answer b
  deriving (Show,Eq)

initial :: Store a b
initial = Store (\key -> Undefined)

value :: Store a b -> a -> Perhaps b
value (Store f) key = f key  

update :: Eq a => Store a b -> a -> b -> Store a b
update (Store f) key value =
  Store (\k -> if key == k then (Answer value) else f k)

-- f is a function that maps keys to values
-- f represents that state of the store before the update

-- we update the store by assigning the value value to the key key
-- the function f' that represents the state of the store after the update
-- f' is related to f as follows:

-- f' k
--   | k == key  = (Answer value)
--   | otherwise = f k

-- instead of calling the new function f' we can just use a lambda function (anonymous function)


