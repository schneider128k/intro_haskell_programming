module Store
  ( Store,
    initial, -- Store
    value,   -- Store -> Var -> Integer
    update,  -- Store -> Var -> Integer -> Store
  ) where

type Var = String

newtype Store = Store (Var -> Integer)

initial :: Store 
initial = Store (\v-> 0)

value :: Store -> Var -> Integer
value (Store sto) v = sto v  


update :: Store -> Var -> Integer -> Store
update (Store sto) w n =
  Store (\v -> if v==w then n else sto v)

