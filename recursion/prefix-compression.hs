subString :: String -> String -> String 
subString [] _ = []
subString _ [] = []
subString (x:xs) (y:ys) 
    | x == y = [x] ++ subString xs ys 
    | otherwise = []

solve :: [String] -> String 
solve (s1:s2:_) = 
    (show l_ss ++ " " ++ ss) ++ "\n" ++ 
    (show (length r_s1) ++ " " ++ r_s1) ++ "\n" ++
    (show (length r_s2) ++ " " ++ r_s2)
    where 
        ss = subString s1 s2 
        l_ss = length ss 
        r_s1 = drop l_ss s1 
        r_s2 = drop l_ss s2

main = interact $ solve . words