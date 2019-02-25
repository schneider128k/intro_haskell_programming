import Data.Char

capitalize :: String -> String
capitalize = map toUpper

readEcho :: IO ()
readEcho =
  do
    line <- getLine
    putStrLn (capitalize line)

main = readEcho


