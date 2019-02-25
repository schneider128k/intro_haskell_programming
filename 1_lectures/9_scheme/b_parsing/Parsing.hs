import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left  err -> "no match: " ++ show err
  Right val -> "match: " ++ show val

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

  
  

