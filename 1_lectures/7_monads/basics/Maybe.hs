class MyMonad m where
  (>>-)  :: m a -> (a -> m b) -> m b
  ret    :: a -> m a

instance MyMonad Maybe where

  (Just x) >>- k = k x
  Nothing  >>- k = Nothing
  ret x          = Just x


test :: Maybe Int
test =
  (Just 10) >>- (\x -> ret (x + 20))

test2 :: Maybe Int
test2 =
  Nothing >>- (\x -> ret (x + 20))

test3 :: Maybe Int
test3 =
  (Just 10) >>- \x ->
  Nothing   >>- \y ->
  ret (x+y)

test4 :: Maybe Int
test4 =
  (Just 10) >>- \x ->
  (Just 20) >>- \y ->
  ret (x+y)
