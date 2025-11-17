import Data.Char (digitToInt)

digit_sum :: Integer -> Integer 
digit_sum x = sum $ map (toInteger . digitToInt) $ show x

super_digit :: Integer -> Integer 
super_digit x
    | x < 10 = x 
    | otherwise = super_digit $ digit_sum x 

solve :: [Integer] -> Integer
solve (x:n:_) = super_digit $ (digit_sum x) * n

main = interact $ show . solve . map read . words