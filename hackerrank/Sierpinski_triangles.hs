
triangle :: Int -> Int -> Bool
triangle x y =
  abs x <= y

triangleMask :: Int -> Int -> Int -> Bool
triangleMask x y 0 =
  triangle (x - 31) y

triangleMask x y n =
  let y' = 32 `div` (2 ^ n)
      x' = 64 `div` (2 ^ n)
  in
  triangle (((x + (y `div` y') * y' + y') `mod` x') - y' + 1) (y `mod` y')

triangleMaskChain x y 0 =
  triangleMask x y 0
triangleMaskChain x y n =
  triangleMask x y n && triangleMaskChain x y (n - 1)
 
draw f n =
  let w = [0..62]
      h = [0..31]
  in
  map (\y -> map (\x ->
    if (triangleMaskChain x y n)
    then '1' else '_') w) h


main = do
  raw <- getLine
  let n = read raw :: Int
  mapM_ putStrLn (draw triangle n)

