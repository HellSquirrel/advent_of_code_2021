module Main where

import System.IO
import Control.Monad
import Data.Maybe
import Data.List
import Data.Bifunctor(bimap, first)


toI :: String -> [Integer]
toI = read

breedPeriod = 6
initialTimer = 8

generationsSplit :: Integer -> Integer -> [Integer]
generationsSplit totalDays initialDays = [initialDays + 1, initialDays + 2 + breedPeriod .. totalDays]

-- nextGenerationCount :: Integer -> Integer -> [(Integer, Integer)]
-- nextGenerationCount totalDays initialDays =
--     let days = generationsSplit totalDays initialDays
--     in 1 + foldr ((+) . \d -> nextGenerationCount totalDays (initialTimer + d)) 0 days

dec value = if value == 0 then breedPeriod else value - 1

-- getFry :: [(Integer, Integer )] -> [(Integer, Integer)]
-- getFry list =  case lookup breedPeriod list of
--     Nothing -> []
--     Just a -> [(initialTimer, a)]

mergeBreedPeriods :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeBreedPeriods list = remaining ++ ([(breedPeriod, totalWithBreedPeriod) | totalWithBreedPeriod /= 0])
    where totalWithBreedPeriod = sum $ map snd $ filter (\(value, _) -> value == breedPeriod) list
          remaining = filter (\(value, _) -> value /= breedPeriod) list


tick :: [(Integer, Integer )] -> [(Integer, Integer)]
tick = mergeBreedPeriods . concatMap (\(value, count) -> if value /= 0 then [(value - 1, count)] else [(breedPeriod, count), (initialTimer, count)])

breed :: Int -> [(Integer, Integer )] -> [(Integer, Integer)]
breed 0 list = list
breed days list = breed (days - 1) (tick list)

flockSize :: [(Integer, Integer )] -> Integer
flockSize = sum . map snd

-- flockSize :: Integer -> Integer -> Integer
-- flockSize initialDay totalDays 
--     | initialDay >= totalDays = 1
--     | otherwise = generationCount initialDay totalDays

-- collectiveBreeds initialFlock totalDays = length initialFlock + sum (map (totalBreedsAfter totalDays) initialFlock)

processList :: [Integer] -> [(Integer, Integer )]
processList = foldr (\v acc -> if isJust (lookup v acc)
    then map (\(val, count) -> if val == v then (val, count + 1) else (val, count)
        ) acc
    else (v, 1) : acc)
        []

main :: IO ()
main = do
    handle <- openFile "./day6" ReadMode
    contents <- hGetContents handle
    let initialPopulation = processList $ toI $ "[" ++ contents ++ "]"
    -- print $ breed 1 initialPopulation
    -- print $ breed 2 initialPopulation
    -- print $ breed 3 initialPopulation
    print $ flockSize $ breed 256 initialPopulation
    hClose handle
