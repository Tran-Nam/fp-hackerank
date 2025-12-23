type Point = (Double, Double)

parse :: [Double] -> [Point]
parse (x:y:xs) = [(x, y)] ++ parse xs 
parse _ = []

cal_length :: Point -> Point -> Double
cal_length (x1, y1) (x2, y2) = ((x1-x2)**2 + (y1-y2)**2)**0.5

cal_perimeter :: [Point] -> Double 
cal_perimeter ps = sum $ zipWith cal_length ps (t ++ h)
    where   t = tail ps 
            h = [head ps]

main = interact $ show . cal_perimeter . parse . map read . tail . words