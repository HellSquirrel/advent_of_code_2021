{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Control.Monad ()
import Data.Maybe
import Data.Char(isLower)
import Data.List(elemIndex)
import Data.Graph (edges)

newtype Graph a = Graph [(a, a)] deriving (Eq, Show, Functor)
type Road  =  [String]
type Edge = (String, String)

splitBy :: String -> (String, String) -> Bool -> String -> Edge
splitBy splitter acc flag [] = acc
splitBy splitter (left, right) flag (x:xs)
    | head splitter == x = splitBy splitter (left, right) True (drop (length splitter - 1) xs)
    | otherwise = splitBy splitter nextAcc flag xs
        where nextAcc = if flag then (left, right ++ [x]) else (left ++ [x], right)

nextEdges :: String -> Graph String -> [Edge]
nextEdges "end" _= []
nextEdges e (Graph edges) = filter (\el@(first, second) ->  first == e || second == e) edges

getNext :: String -> Edge -> String
getNext cave (a,b) = if a == cave then b else a

countCaves :: String -> Road -> Int
countCaves caveName = length . filter (==caveName)

isValidCave :: String -> Road -> Bool
isValidCave caveName road
    | not (isLower (head caveName)) = True
    | otherwise = countCaves caveName road == 0

isValidCave2 :: String -> Road -> Bool
isValidCave2 caveName road
    | caveName == "start" = False 
    | not (isLower (head caveName)) = True
    | otherwise = countCaves caveName road < 2

isCycle :: Road -> Edge -> Bool
isCycle (step4:step3:step2:step1:_) (start, end)
    | step4 == step2 && step3 == step1 = start == step3 && end == step4 || start == step4 && end == step3
    | otherwise = False
isCycle _ _ = False

notCycle :: Edge -> String -> Road -> Bool
notCycle edge lastCave road
    | length road < 4 = True
    | otherwise = not (isCycle (take 4 road) edge)


nextEdgesForRoad :: Road -> Graph String -> [Edge]
nextEdgesForRoad [] grapqh = []
nextEdgesForRoad road@(lastCave:rest) graph = [
    edge | edge@(start,end) <- allEdges,
    isValidCave2 (getNext lastCave edge) road,
    notCycle edge lastCave road]
    where
        allEdges = nextEdges lastCave graph

proceed :: Graph String  -> Road -> [Road]
proceed graph [] = []
proceed graph road@(lastCave: _) = if null nextForRoad then [road] else (++) <$> map ((:[]) . getNext lastCave) nextForRoad <*> [road]
    where nextForRoad = nextEdgesForRoad road graph


proceedRoads :: Graph String  -> [Road] -> [Road]
proceedRoads graph = concatMap (proceed graph)

proceedToTheEnd :: Int -> Graph String  -> [Road] -> [Road]
proceedToTheEnd 20 graph acc =  acc
proceedToTheEnd n graph acc = proceedToTheEnd (n + 1) graph nextRoads
    where nextRoads = proceedRoads graph acc

findValidRoads = filter ((=="end") . head)

main = do
    handle <- openFile "./day12-short" ReadMode
    contents <- hGetContents handle
    let lst = map (splitBy "-" ([], []) False) $ words contents
    let g = Graph lst
    -- let firstSteps = map ((:["start"]) . getNext "start") (nextEdges "start" g)
    let roads = findValidRoads $ proceedToTheEnd 0 g [["start"]]
    print $ length roads
    putStrLn $ take 1000 $ unlines $ map (concatMap (++ ",") . reverse) roads
    hClose handle
