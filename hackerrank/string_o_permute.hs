
permute :: String -> String
permute "" =  ""
permute [a,b] = [b, a]
permute (a:b:tl) =
  b:a:(permute tl)

main = do
  raw <- getContents
  let input = tail $ lines raw
  putStr $ unlines $ map permute input

