
module Main where


gcd' :: Integral a => a -> a -> a
gcd' n m = -- Complete this function
  if m `mod` n == 0 then
    n
  else
    gcd' (m `mod` n) n


-- This part is related to the Input/Output and can be used as it is
-- Do not modify it
main = do
  input <- getLine
  print . uncurry gcd' . listToTuple . convertToInt . words $ input
 where
  listToTuple (x:xs:_) = (x,xs)
  convertToInt = map (read :: String -> Int)

