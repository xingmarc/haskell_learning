
dedupe :: String -> String
dedupe [] = []
dedupe [a] = [a]
dedupe (x:xs) =
  let filtered = filter (\s -> s /= x) xs
  in
  [x] ++ dedupe filtered

main = do
  raw <- getLine
  putStrLn $ dedupe raw
