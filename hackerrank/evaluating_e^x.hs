import Control.Applicative
import Control.Monad
import System.IO

e :: Double -> [Double]
e x = [x**y/product[1..y] | y <- [0..]]

solve :: Double -> Double
solve x = sum $ take 10 (e x)


main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \a0  -> do
        x_temp <- getLine
        let x = read x_temp :: Double
        print (solve x)


getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret 

