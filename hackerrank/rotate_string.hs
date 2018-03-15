

rotate :: String -> [String]
rotate [] = []
rotate (hd:tl) =
  [tl ++[hd]] ++ (rotate (tl ++ [hd]))

main = do
  raw <- getContents
  let input = tail $ lines $ raw
  mapM_ putStrLn (map (\s -> (unwords (take (length s) (rotate s)))) input)
  -- print $ take 5 (rotate "abcde")

