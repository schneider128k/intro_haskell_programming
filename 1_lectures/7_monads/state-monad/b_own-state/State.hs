module State where

-- my own implementation of the state monad

class MyMonad m where
  ret   :: a -> m a
  (>>-) :: m a -> (a -> m b) -> m b

--      Operation
--      Instruction
newtype State s a = State { runState :: s -> (a, s) }

instance MyMonad (State s) where
  ret x = State $ \s -> (x, s)


  (State h) >>- f = (State g)
    where
      g s = runState (f a) s'
        where
          (a, s') = h s

-- alternative implementation using let
--
--    (State h) >>- f = State $ \s ->
--                        let (a, s') = h s
--                        in runState (f a) s'
