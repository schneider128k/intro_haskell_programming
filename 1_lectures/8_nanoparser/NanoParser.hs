module NanoParser where

import Data.Char
import Control.Monad
import Control.Applicative

-- the core of the parser combinator machinery

newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error "Parser did not consume entire stream."
    _           -> error "Parser error"

item :: Parser Char
item = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c,cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(x, s') -> parse (f x) s') $ parse p s

unit :: a -> Parser a
unit a = Parser $ \s -> [(a, s)]

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> [(f a, s1) | (a, s1) <- p s]

instance Applicative Parser where
  pure = return
  (Parser p1) <*> (Parser p2) =
    Parser $ \s ->  [ (f a, s2)  | (f, s1) <- p1 s, (a, s2) <- p2 s1]

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p1 p2 = Parser $ \s -> parse p1 s ++ parse p2 s

failure :: Parser a
failure = Parser (\_ -> [])

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    []  -> parse q s
    res -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c
    then unit c
    else failure

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do { a <- p; rest a }
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
          <|> return a

-- several combinators for detecting common patterns of characters
-- (numbers, parenthesized expressions, whitespace, etc.) 

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs) }

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a }

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n





  
