module Main where 

mingle :: String -> String -> String 
mingle xs [] = []
mingle [] ys = []
mingle (x:xs) (y:ys) = x:y:mingle xs ys

main :: IO()
main = do 
    a <- getLine
    b <- getLine
    putStrLn $ mingle a b
