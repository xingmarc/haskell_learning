-- Area Under Curves and Volume of Revolving a Curve
-- Sample input:
-- 1 2 3 4 5
-- 6 7 8 9 10
-- 1 4  
--





item :: Double -> (Int, Int) -> Double
item x (a, b) = fromIntegral a * (x ** (fromIntegral b))

compute:: [Int] -> [Int] -> Double -> Double
compute a b x = sum $ map (item x) $ zip a b

circle:: Double -> Double
circle r = r * r * 3.1415926::Double

compute':: [Int] -> [Int] -> Double -> Double
compute' a b x = circle $ compute a b x

doIntegral:: Double -> Double -> (Double -> Double) -> Double
doIntegral l r func = sum [func x | x <- [l, l + 0.001..r]]

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = -- sum  [compute a b x | x <- [fromIntegral l, fromIntegral l + 0.001..fromIntegral r]::[Double]]
    let lb = fromIntegral l
        rb = fromIntegral r
    in map (/1000.0) [doIntegral lb rb $ compute a b, doIntegral lb rb $ compute' a b]
   
-- solve :: Int -> Int -> [Int] -> [Int] -> [Double]
-- solve l r a b = 
