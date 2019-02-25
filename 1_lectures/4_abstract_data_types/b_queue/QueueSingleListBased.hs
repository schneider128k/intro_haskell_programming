module Queue
  ( Queue ,
    emptyQ ,    -- QueueQ a
    isEmptyQ,   -- QueueQ a -> Bool
    addQ ,      -- a -> Queue a -> Queue a
    remQ        -- Queue a -> ( a , Queue a )
  ) where

newtype Queue a = Queue [a]

emptyQ :: Queue a
emptyQ = Queue []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Queue []) = True
isEmptyQ _          = False

addQ :: a -> Queue a -> Queue a
addQ x (Queue xs) = Queue (xs++[x])               -- O(n)

remQ :: Queue a -> ( a , Queue a )                -- O(1)
remQ q@(Queue xs)
  | not (isEmptyQ q) = (head xs, Queue (tail xs))
  | otherwise        = error "remQ"

