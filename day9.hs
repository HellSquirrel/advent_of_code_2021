import System.IO  
import Control.Monad
import Data.Char(digitToInt)

parse :: String -> [[Int]]
parse = map (map digitToInt) . words

isLowest x y input = 

lowest input = [x | x <- [0..length input], y <- [0..length input]]

main = do  
    handle <- openFile "./day9" ReadMode
    contents <- hGetContents handle
    let input = parse contents
    print input
    hClose handle   