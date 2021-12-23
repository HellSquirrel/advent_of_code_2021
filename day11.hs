import System.IO
import Control.Monad
import Data.Char(digitToInt, intToDigit)

mapWithValue :: Int -> (t -> Int -> a)  -> [t] -> [a]
mapWithValue _ fn  []  = []
mapWithValue index fn (x: xs) = fn x index: mapWithValue (index + 1) fn xs

mapWithIndex = mapWithValue 0

prepare = mapWithIndex (\row y -> mapWithIndex (\value x -> (False, value)) row)

countFlashes :: Int -> Int -> [[(Bool, Int)]] -> Int
countFlashes x0 y0 field = sum [if fst (field !! y !! x) then 1 else 0 |
   x <- [x0 - 1 .. x0 + 1],
   x >= 0,
   y <- [y0 - 1 .. y0 + 1],
   y >= 0,
   x < length field,
   y < length field]

rechargeCell (flashing, value) x y field
    | flashing = (False, 0)
    | value == 0 = (False, 0)
    | otherwise = (False, value + countFlashes x y field)

acc = map (map (fmap (+1)))
flash = map (map (\(flashing, value) -> (not flashing && value > 9, value )))

recharge :: [[(Bool, Int)]] -> [[(Bool, Int)]]
recharge field = mapWithIndex (\row y -> mapWithIndex (\cell x -> rechargeCell cell x y field) row) field

detonate :: [[(Bool, Int)]] -> [[(Bool, Int)]]
detonate field =
    let next = recharge . flash $ field
    in if next == field then field else detonate next

countTotalFlashes :: [[(Bool, Int)]] -> Int
countTotalFlashes field = sum [if snd (field!!x!!y) == 0 then 1 else 0 | x <- [0..length field - 1], y <- [0..length field - 1]]


steps :: Int -> Int -> Int -> [[(Bool, Int)]] -> (Int, [[(Bool, Int)]])
steps n count maxCount field
    | n == maxCount = (count, field)
    | otherwise = steps (n + 1) (count + countTotalFlashes next) maxCount next
        where next = detonate $ acc field

total = sum . map (sum . map snd)

autoSteps :: Int -> [[(Bool, Int)]] -> Int
autoSteps n field
    | total field == 0 = n
    | otherwise = autoSteps (n + 1) next
        where next = detonate $ acc field


showField :: [[(Bool, Int)]] -> String
showField = concatMap ((++ "\n") . map (intToDigit . snd))

main :: IO ()
main = do
    handle <- openFile "./day11" ReadMode
    contents <- hGetContents handle
    let input = prepare $ map (map digitToInt) $ words contents
    let (count, field) = steps 0 0 95 input
    putStr $ showField field
    print "----"
    print count

    print $ autoSteps 0 input

    hClose handle
