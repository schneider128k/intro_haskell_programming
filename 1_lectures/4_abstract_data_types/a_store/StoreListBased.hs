module Store
  ( 
    initial, -- Store
    value,   -- Store -> Var -> Integer
    update,  -- Store -> Var -> Integer -> Store
  ) where

type Var = String

newtype Store = Store [ (Integer,Var) ] -- variable "a" holds the value 5 [(5,"a")]
                                        -- value, key

initial :: Store 
initial = Store []

value :: Store -> Var -> Integer
value (Store []) _ = 0
value (Store ((n,w):sto)) v
  | v == w    = n
  | otherwise = value (Store sto) v

update :: Store -> Var -> Integer -> Store
update (Store sto) v n = Store ((n,v):sto)
