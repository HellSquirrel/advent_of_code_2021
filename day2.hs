import System.IO  
import Control.Monad

coord :: ([Char], Int) -> (Int, Int)
coord ("up", x) = (0, -x)
coord ("down", x) = (0, x)
coord ("forward", x) = (x, 0)

rList = do  
    handle <- openFile "./day2" ReadMode
    contents <- hGetContents handle
    let lst = words contents
    let commands =  [coord (lst !! x, read $ lst !! (x + 1) )| x <- [0, 2 .. length lst - 2]]
    let (h, d, a) = foldl (\ (hor, depth, aim) (x, y) -> (hor + x, depth + x * aim , aim + y)) (0, 0, 0) commands
    print commands
    print(h * d)
    print(h, d, a)
    hClose handle   
