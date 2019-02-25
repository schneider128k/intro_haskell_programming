data Perhaps a = Fail | Value a
  deriving (Show,Eq)

class MyMonad m where
  (>>-)  :: m a -> (a -> m b) -> m b
  ret    :: a -> m a

instance MyMonad Perhaps where

  (Value x) >>- k = k x          -- a -> Perhaps b; for example, a String, b Float
  Fail      >>- k = Fail
  ret x           = Value x


test :: Perhaps Int
test =
--Perhaps Int    Int -> Perhaps Int  
  (Value 10) >>- (\x -> ret (x + 20))

test2 :: Perhaps Int
test2 =
  Fail >>- (\x -> ret (x + 20))

test3 :: Perhaps Int
test3 =
  (Value 10) >>- \x ->
  Fail >>- \y ->
  ret (x+y)


data List a = Empty | Cons a (List a)
  deriving (Show,Eq)

           
