import System.IO  
import Control.Monad

main = do  
    handle <- openFile "./day4-short" ReadMode
    contents <- hGetContents handle
    let input = words contents
    print $ head input
    print $ input !! 1
    hClose handle   