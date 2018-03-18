
removeChar :: Char -> String -> String
removeChar _ [] = []
removeChar m (x:xs)
    | m == x = removeChar m xs
    | otherwise = x : (removeChar m xs)


reduction :: String -> String
reduction [] = []
reduction (x:xs) = x : (reduction (removeChar x xs))

main = do
    input <- getLine
    putStrLn $ reduction input

