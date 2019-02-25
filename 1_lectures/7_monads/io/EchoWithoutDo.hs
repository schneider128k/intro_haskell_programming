import Data.Char

capitalize :: String -> String
capitalize = map toUpper

readEcho :: IO ()
readEcho =
    getLine >>= \line ->
    putStrLn (capitalize line)

main = readEcho


