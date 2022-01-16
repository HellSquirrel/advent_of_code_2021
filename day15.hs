{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode))
import Control.Monad
import Data.Maybe ( fromJust )
import Data.Char(digitToInt)
import Data.List(delete)
import Debug.Trace (trace)


type Coords = (Int, Int)
type DistEntry = (Int, Int)
type AreaMap = [(Coords, DistEntry)]


inputWithDistances :: [[Int]] -> AreaMap
inputWithDistances = (((0, 0), (0, 0)):) . tail . concatMap (
    \(r, list) -> zipWith (\ c v -> ((r, c), (v, 500))) [0..] list
    ) . zip [0..]

getClosestKeys :: Coords -> Coords -> [Coords]
getClosestKeys (row, col) (maxRow, maxCol)
    | maxRow == row && maxCol == col = []
    | maxRow == row = [(row, col + 1)]
    | maxCol == col = [(row + 1, col)]
    | otherwise  = [(row + 1, col), (row, col + 1)]

getClosestKeysReverted :: Coords -> [Coords]
getClosestKeysReverted (row, col)
    | (row, col) == (0, 0) = []
    | row == 0 = [(row, col - 1)]
    | col == 0 = [(row - 1, col)]
    | otherwise  = [(row - 1, col), (row, col - 1)]

findMin :: AreaMap -> (Coords, DistEntry)
findMin m | trace ("--> " ++ show m) False = undefined
findMin m = foldr getMin (head m) m

getClosestMinElement :: [Coords] -> AreaMap -> (Coords, DistEntry)
getClosestMinElement coords m = findMin $ map (\c -> (c, fromJust (lookup c m))) coords


updMap :: Int -> Coords  -> AreaMap -> AreaMap
updMap mdist (mrow, mcol) = map (\entry@((r, c), (dig, dist)) -> if r == mrow && c == mcol then ((r, c), (dig, dig + mdist)) else entry)

getMin :: (Coords, DistEntry) -> (Coords, DistEntry) -> (Coords, DistEntry)
getMin el1@(_, (_, d1)) el2@(_, (_, d2)) = if d1 < d2 then el1 else el2



moveMin :: (Int, Int) -> (AreaMap, AreaMap)  -> (AreaMap, AreaMap) 
moveMin constrains (remaining, visited) = 
    let minEl@((mrow, mcol), (_, mdist)) = findMin remaining 
        closestKeys = getClosestKeys (mrow, mcol) constrains
        mapWithoutMin = filter (/=minEl) remaining
        updatedMap = foldr (updMap mdist) mapWithoutMin closestKeys
    in (updatedMap,  minEl:visited) 

findShortestPath :: (Int, Int) -> (AreaMap, AreaMap)  -> (AreaMap, AreaMap)
findShortestPath constrains (remaining, visited)
    | null remaining = (remaining, visited)
    | not (null remaining) && (constrains == fst (head remaining)) = (remaining, visited)
    | otherwise = findShortestPath constrains $ moveMin constrains (remaining, visited)

getPath :: Coords -> AreaMap -> AreaMap -> AreaMap
getPath constrains [] m = getPath constrains (filter ((== constrains) . fst) m) m
getPath constrains (lastEl:acc) m 
    | fst lastEl == (0, 0) = lastEl:acc
    | otherwise =
        let (coords, _) = lastEl
            closest = getClosestMinElement (getClosestKeysReverted coords) m
        in getPath constrains (closest:lastEl:acc) m
    


main :: IO ()
main = do
    handle <- openFile "./day15-short" ReadMode
    contents <- hGetContents handle
    let input = map (map digitToInt) $ words contents
    let constrains =  (length input - 1, length (head input) - 1)
    let initialMaps = (inputWithDistances input, [])
    let move = moveMin constrains

    -- print $ fst initialMaps
    -- print "---->"
    -- print $ move initialMaps
    -- print "---->"
    -- print $ move $ move initialMaps
    -- print "---->"
    -- print $ move $ move $ move initialMaps
    -- print $ foldr (const move) initialMaps [0..25]
    print $ snd $ findShortestPath constrains initialMaps
    print $ getPath constrains [] $ snd $ findShortestPath constrains initialMaps
    hClose handle
