
isPrime :: Int -> Bool
isPrime x = null [y | y <- [2..floor (sqrt (fromIntegral x))], x `mod` y == 0]

fate :: Int -> String
fate n =
  "HHHH"
  
main = do
  print $ fate 3137
