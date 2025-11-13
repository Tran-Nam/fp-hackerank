permute :: String -> String 
permute [] = []
permute [x] = [x]
permute (x:y:xs) = [y, x] ++ permute xs

main = interact $ unlines . map permute . tail . lines
