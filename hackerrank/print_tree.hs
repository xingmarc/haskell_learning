
stem x y =
  x == 0

branch x y =
  abs x - 1 == y

tree x y y0 l l' =
  if y >= y0 && y < l then stem x y else if y >= l && y < l' then branch x (y - l) else False


-- y' : 0, 32, 32 + 16 = 48, 48 + 8 = 56,56 + 4 = 60
--      0  32            16            8           4
--      1  2             3             4           5
-- x' : 0  16            8             4           2
treeMask x y 1 =
  tree x y 0 16 32
treeMask x y n =
  let p = 2 ^ (5 - n)
      m = [0,0,32,48,56,60]
  in
  tree (x - 2*p) y (m!!n) ((m!!n) + p) ((m!!n) + 2*p) || tree (x + 2*p) y (m!!n) ((m!!n) + p) ((m!!n) + 2*p) 
    || treeMask x y (n - 1)

draw n =
  let w = [0..99]
      h = [0..62]
  in
  map (\y -> map (\x -> if treeMask (x - 48) y n then '1' else '_') w) h


main = do
  raw <- getLine
  let n = read raw :: Int
  mapM_ putStrLn (draw n)
