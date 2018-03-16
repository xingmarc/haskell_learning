import Control.Monad
import Data.List( sort )
import Data.IntMap hiding( map )

formatOutput Nothing = -1
formatOutput (Just (_, n)) = n

solve arr inps =
  let arr' = reverse (sort arr) -- descending order
      cumsums = scanl (+) 0 arr'
      m = fromAscList $ zip cumsums [0..]
  in
  map (formatOutput . flip lookupGE m) inps

main = do
  _ <- getLine
  inputRaw <- getLine
  _ <- getLine
  raw <- getContents
  let arr = map (\s -> read s::Int) (words inputRaw)
      nums = map (\s -> read s::Int) (lines raw)
      ans = solve arr nums
  mapM_ print ans

