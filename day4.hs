{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.IO
import Control.Monad
import Data.Char(digitToInt, isDigit)
import Data.List (filter, find)
import Data.Text(splitOn,pack, unpack, Text)
import Data.Maybe(isJust)

getSequence = map digitToInt . filter (/=',') . head


type Board = [(Int, (Int, Int, Bool))]

enumerateBoard :: [Int] -> Board
enumerateBoard board = zip board [(x, y, False) | x <- [0..5], y <- [0..5]]

getBoards :: String -> [Board]
getBoards = map (enumerateBoard . map digitToInt . filter isDigit . unpack) . tail . splitOn (pack "\n\n") . pack

mark :: Int -> [Board] -> [Board]
mark number = map (map (\(el, (x, y, _)) -> (el, (x, y, el == number))))

getMark (_, (_, _, marked)) = marked

bump :: Int -> [Int] -> [Int]
bump rowN acc = take (rowN + 1) acc ++ [acc!!rowN + 1] ++ drop (rowN + 1) acc

winRows :: Board -> Bool
winRows = (>4) . maximum . foldr bump (replicate 0 5) . map (\(_, (x, _, _)) -> x) . filter getMark

winCols :: Board -> Bool
winCols = (>4) . maximum . foldr bump (replicate 0 5) . map (\(_, (_, y, _)) -> y) . filter getMark

getWinningBoard :: [Board] -> Maybe Board
getWinningBoard = find (\l -> winRows l ||  winCols l)


play :: [Int] -> [Board] -> Board
play (x: xs) boards = 
    let boardsWithMark = mark x boards 
        winningBoard = getWinningBoard boardsWithMark
    in case winningBoard of 
        Just b -> b
        _ -> play xs boardsWithMark


main = do
    handle <- openFile "./day4-short" ReadMode
    contents <- hGetContents handle
    let input = words contents
    -- print $ getSequence input
    print $ play (getSequence input) (getBoards contents)
    hClose handle