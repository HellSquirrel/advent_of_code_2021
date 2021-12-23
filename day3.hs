import System.IO  
import Control.Monad

rList = do  
    handle <- openFile "./day3" ReadMode
    contents <- hGetContents handle
    let input = words contents
    print $ head input
    hClose handle   
