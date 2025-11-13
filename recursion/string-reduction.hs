reduction :: String -> String 
reduction [] = []
reduction (x: xs) = [x] ++ reduction ys 
    where
        ys = filter (\a -> a /= x) xs

main = interact $ unlines . map reduction . lines
