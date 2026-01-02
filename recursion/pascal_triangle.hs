factorial :: Integer -> Integer
factorial n = go n 1
    where 
        go :: Integer -> Integer -> Integer 
        go 0 acc = acc 
        go n acc = go (n-1) (acc * n)

pascal :: Integer -> [Integer]
pascal n = [(factorial n) `div` ((factorial i) * (factorial $ n-i)) | i <- [0..n]]

pascal_triangle :: [[Integer]]
pascal_triangle = [pascal n | n <- [0..]]

main :: IO()
main = do 
    input <- getLine
    let n = read input :: Int 
    mapM_ (putStrLn . unwords . map show) (take n pascal_triangle)