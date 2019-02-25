-- you have to be very careful when designing
-- purely functional data structures

-- this is an example of how to implement a queue in Haskell
-- this implementation mitigates the problem that only elements
-- at the beginning of a list can be access efficiently.

-- The performance of this data structure can be analyzed using
-- amortized analysis (average performance vs worst case).

-- we do any sequence of t operations op1, op2, ..., opn
-- avgtime = 1/n * (time(op1) + ... + time(op2))

module Queue
  ( Queue ,
    emptyQ ,    -- QueueQ a
    isEmptyQ,   -- QueueQ a -> Bool
    addQ ,      -- a -> Queue a -> Queue a
    remQ        -- Queue a -> ( a , Queue a )
  ) where

data Queue a = Queue [a] [a]
  deriving (Show,Eq)

emptyQ :: Queue a
emptyQ = Queue [] []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Queue [] []) = True
isEmptyQ _             = False

addQ :: a -> Queue a -> Queue a
addQ x (Queue xs ys) = Queue xs (x:ys)                    -- O(1)

remQ :: Queue a -> ( a , Queue a )                        
remQ (Queue [] [])     = error "remQ"
remQ (Queue (x:xs) ys) = (x, Queue xs ys)                 -- O(1)
remQ (Queue [] ys)     = remQ (Queue (reverse ys) [])     -- O(n) n is the number of elements in second list     


