import Data.List

f :: [String] -> [Int]
f = map read

-- List of chars to int

genPass :: String -> String -> String
genPass frase lista = take 12 [b | (b,i) <- zip frase [0..], x <- f [[x] | x <- lista], x == i, x /= ' ']

main = do
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine :: IO String
    foo <- putStrLn "Hello, what's your number?"
    number <- getLine :: IO String
    putStrLn ("Hey " ++ (genPass name number) ++ ", you rock!")
