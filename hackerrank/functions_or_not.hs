import qualified Data.Map.Strict as MS
import Data.List

insertMap :: (Ord a) => MS.Map a [a] -> [a] -> MS.Map a [a]
insertMap m e
  | MS.member nk m = MS.insert nk (nub (nv : cv)) rm
  | otherwise = MS.insert nk [nv] m
  where nk = head e
        nv = last e
        cv = m MS.! nk
        rm = MS.delete nk m

getMapping m = foldl (\m e -> insertMap m e) MS.empty m
isValidFunc m = nonUniqueLen == 0
  where mapping = getMapping m
        nonUnique = filter (\e -> length e > 1) (MS.elems mapping)
        nonUniqueLen = length nonUnique

mapToInts lines = map (\e -> map (\n -> read n :: Int) (words e)) lines

testCase c [] = reverse c
testCase c lines = testCase (tc : c) rst
  where tcc = read (head lines) : Int -- count
        tc = mapToInts $ take tcc $ tail lines
        rst = drop (tcc + 1) lines
main = do
  raw <- getContents
  let lns = tail $ lines raw
      defs = testCase [] lns
      res = map isValidFunc defs
      isYesOrNo = map (\b -> if b then "YES" else "NO") res
  mapM_ putStrLn isYesOrNo

