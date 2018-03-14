
sumUp :: [Int] -> [Int]
sumUp [] = []
sumUp [_] = []
sumUp (hd:tl) =
  hd + (head tl) : sumUp tl

pascalTriangle :: Int -> [[Int]]
pascalTriangle 1 = [[1]]
pascalTriangle 2 = [[1], [1,1]]
pascalTriangle n =
  let prev = pascalTriangle (n - 1)
  in
  prev ++ [([1] ++ (sumUp (last prev)) ++ [1])]

toString :: [[Int]] -> String
toString t =
  unlines (map (\s -> (unwords (map show s))) t)

main = do
  print $ toString $ pascalTriangle 5
