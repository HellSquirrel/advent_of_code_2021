import System.IO
import Control.Monad
import Data.Maybe

import System.IO
import Data.List

initialTotals = [('N', 0), ('C', 0), ('B', 0)]




pattern = "NNCB"

-- pattern = "BVBNBVPOKVFHBVCSHCFO"

createRuleMap l = [t | t <- zip l (drop 2 l), length (fst t) == 2 ]
applyRule [x,y] [z] = [x, z, y]

step :: String -> String -> [(String, String)] -> String
step acc [] rules = acc
step acc [x] rules = acc ++ [x]
step acc (x:y:rest) rules = step (acc ++ take 2 (applyRule [x,y] (fromJust $ lookup [x,y] rules))) (y:rest) rules

steps 9 input = step [] pattern input
steps n input = step [] (steps (n + 1) input) input


count str c = length $ filter (== c) str

totals str = 
    let l = filter (/= 0) $ sort $ map (count str) ['A'..'Z']
    in (last l - head l)

main :: IO ()
main = do
    handle <- openFile "./day14-short" ReadMode
    contents <- hGetContents handle
    let input = createRuleMap $ words contents
    -- print $ steps 0 input
    print $ totals $ steps 0 input
    hClose handle
