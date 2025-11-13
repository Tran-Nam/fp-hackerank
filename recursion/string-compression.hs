import Data.Char (chr, ord)

compress :: String -> String 
compress [] = []
compress (x:xs) 
    | lengthList == 0 = [x] ++ compress ys
    | otherwise = (x: nx) ++ compress ys
    where  
        listSameChar = takeWhile (\a -> a == x) xs 
        lengthList = length listSameChar 
        nx = show $ (lengthList + 1)
        ys = dropWhile (\a -> a == x) xs

main = interact $ unlines . map compress . lines
