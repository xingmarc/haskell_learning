-- Filter Positions in a List
-- Remove elements at odd position:
--

f :: [Int] -> [Int]
f [] = []
f [_] = []
f (x:y:xs) = [y] ++ f xs

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
   inputdata <- getContents
   mapM_ (putStrLn. show). f. map read. lines $ inputdata

