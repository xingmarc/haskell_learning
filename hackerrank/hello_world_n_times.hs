import Control.Applicative
import Control.Monad
import System.IO

hello_worlds :: Int -> IO()
hello_worlds 0 = return ()
hello_worlds n = do
    putStrLn "Hello World"
    hello_worlds (n-1)

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    --  Print "Hello World" on a new line 'n' times.
    hello_worlds n

getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          



