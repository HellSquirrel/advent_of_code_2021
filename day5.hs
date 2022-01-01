import System.IO
import Control.Monad
import Data.Maybe
import Data.List
import Data.Bifunctor(bimap)

splitBy :: String -> (String, String) -> Bool -> String -> (String, String)
splitBy splitter acc flag [] = acc
splitBy splitter (left, right) flag (x:xs)
    | head splitter == x = splitBy splitter (left, right) True (drop (length splitter - 1) xs)
    | otherwise = splitBy splitter nextAcc flag xs
        where nextAcc = if flag then (left, right ++ [x]) else (left ++ [x], right)

splitByArrow :: String -> (String, String)
splitByArrow = splitBy " -> " ([], []) False

splitByComma :: String -> (Int, Int)
splitByComma = bimap read read . splitBy "," ([], []) False


-- createRuleMap :: String -> [([Char], [Char])]
-- filter (\((xStart, yStart), (xEnd, yEnd)) -> (xStart == xEnd) || (yStart == yEnd)) .
createRuleMap :: String -> [((Int, Int), (Int, Int))]
createRuleMap = map (bimap splitByComma splitByComma . splitByArrow) . lines

delta :: Num a => ((a, a), (a, a)) -> (a, a) -> a
delta ((xStart, yStart), (xEnd, yEnd)) (x, y) = (xEnd - xStart) * (y - yStart) - (yEnd - yStart) * (x - xStart)

inRange :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inRange ((xStart, yStart), (xEnd, yEnd)) (x, y) = (xMin <= x) && (x <= xMax) && (yMin <= y) && (y <= yMax)
    where
        (xMin, xMax) = if xStart < xEnd then (xStart, xEnd) else (xEnd, xStart)
        (yMin, yMax) = if yStart < yEnd then (yStart, yEnd) else (yEnd, yStart)

fieldSize :: [((Int, Int), (Int, Int))] -> (Int, Int)
fieldSize = foldr (\((xStart, yStart), (xEnd, yEnd)) (xMax, yMax) -> (maximum [xMax, xStart, xEnd], maximum [yMax, yStart, yEnd])) (0, 0)


covers :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
covers rule point = (delta rule point == 0) && inRange rule point

countCoverage :: [((Int, Int), (Int, Int))] -> (Int, Int) -> Int
countCoverage ruleMap coord = foldr (\rule acc -> if covers rule coord then acc + 1 else acc) 0 ruleMap

creteField :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
creteField ruleMap = [(x,y) | x <- [0..xMax], y <- [0..yMax]]
    where (xMax, yMax) = fieldSize ruleMap


main :: IO ()
main = do
    handle <- openFile "./day5" ReadMode
    contents <- hGetContents handle
    let rules = createRuleMap contents
    let field = creteField rules
    print $ length $ filter (>1) $ map (countCoverage rules) field
    hClose handle