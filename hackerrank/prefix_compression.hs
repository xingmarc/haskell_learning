
prefix :: Int -> String -> String -> (Int, String, String)
prefix a [] [] = (a, [], [])
prefix a [] c = (a, [], c)
prefix a b [] = (a, b, [])
prefix p (x1: xs1) (x2:xs2)
  | x1 == x2 = prefix (p + 1) xs1 xs2
  | otherwise = (p, x1:xs1, x2:xs2)

toString :: String -> String
toString s
  | (length s) == 0 = "0"
  | otherwise = show (length s) ++ " " ++ s
  
takeN :: Int -> String -> String
takeN 0 x = []
takeN c (x:xs) = x : (takeN (c-1) xs)

main = do
  x <- getLine
  y <- getLine
  let (a, b, c) = prefix 0 x y
  putStrLn $ (show a ++ " " ++ (takeN a x))
  putStrLn $ toString b
  putStrLn $ toString c

