
mingle :: String -> String -> String
mingle a b = concat $ map (\(s,k) -> [s,k]) (zip a b)

main = do
  raw <- getContents
  let input = lines raw
  putStr $ mingle (head input) (last input)
