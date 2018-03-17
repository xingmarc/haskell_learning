import Data.List

compress' :: [String] -> String
compress' ss =
  let m = map (\a -> if length a == 1 then a else (take 1 a) ++ (show $ length a)) ss
  in
  foldr (++) "" m

compress :: String -> String
compress s = 
  let grp = group s
  in
  compress' grp

main = do
  raw <- getLine
  putStrLn $ compress raw
