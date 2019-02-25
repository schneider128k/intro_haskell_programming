import Prelude hiding (foldl,foldl1)

foldl f s []     = s
foldl f s (x:xs) = foldl f (f s x) xs

foldl1 f (x:xs) = foldl f x xs
foldl1 _ []     = error "empty list"

-- check out
-- https://en.wikibooks.org/wiki/Haskell/Lists_III
-- for discussion of foldr, foldr1, foldl, foldl1

paren :: String -> String -> String
paren x y = "(" ++ x ++ "+" ++ y ++ ")"

-- pass either foldl or foldr as first input
visualizeFold fold s xs =
  fold paren (show s) (map show xs)


