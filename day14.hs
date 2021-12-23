import System.IO
import Control.Monad
import Data.Maybe
import Data.List

data RuleEntry = RuleEntry { pair:: String, symbol:: String, transitionCount :: Int, totalPairs :: Int} deriving Show

createRuleMap l = map (\index -> RuleEntry (l!!index) (l!!(index + 2)) 0 0 )[0, 3 .. (length l - 2)]

pattern = "BVBNBVPOKVFHBVCSHCFO"

toPairs :: String -> [String]
toPairs str = zipWith (\a b -> [a,b]) str (tail str)

findElement :: [RuleEntry] -> String -> String
findElement ((RuleEntry pair nextChar _ _): rest) element
    | pair == element = nextChar
    | otherwise = findElement rest element

breedPair :: [RuleEntry] -> String -> [String]
breedPair ruleMap [a, b] =
    let nextChar = findElement ruleMap [a,b]
    in [a: nextChar, nextChar ++ [b]]

clearPairs :: [RuleEntry] -> [RuleEntry]
clearPairs = map (\(RuleEntry pair symbol transitionCount totalPairs) -> RuleEntry pair symbol transitionCount 0)

addPairTimes :: Int -> [RuleEntry] -> String -> [RuleEntry]
addPairTimes n ruleMap newPair  =
    map (\(RuleEntry pair symbol transitionCount totalPairs) -> if pair == newPair then RuleEntry pair symbol transitionCount (totalPairs + n) else RuleEntry pair symbol transitionCount totalPairs) ruleMap


addTransitionTimes :: Int -> [RuleEntry] -> String -> [RuleEntry]
addTransitionTimes n ruleMap newPair  =
    map (\(RuleEntry pair symbol transitionCount totalPairs) -> if pair == newPair then RuleEntry pair symbol (transitionCount + n) totalPairs else RuleEntry pair symbol transitionCount totalPairs) ruleMap


addAllPairsTimes :: Int -> [RuleEntry] -> [String] -> [RuleEntry]
addAllPairsTimes n ruleMap [] = ruleMap
addAllPairsTimes n ruleMap (pair: xs) = addAllPairsTimes n (addPairTimes n ruleMap pair) xs

step :: [RuleEntry] -> [RuleEntry]
step currentRules =
    let nextRules = clearPairs currentRules
    in foldr (\currentRule@(RuleEntry currentPair _ _ totalPairs) nextRules ->
        let pairs = breedPair currentRules currentPair
            updatedRuleMap = addAllPairsTimes totalPairs nextRules pairs
        in addTransitionTimes totalPairs updatedRuleMap currentPair
        ) nextRules currentRules

addToTotals :: RuleEntry -> [(String, Int)] -> [(String, Int)]
addToTotals (RuleEntry _ symbol transitions _) = map (\(currentSymbol, value) -> if currentSymbol == symbol then (currentSymbol, value + transitions) else (currentSymbol, value))

getTotals :: [RuleEntry] -> [(String, Int)]
getTotals = foldr (\(RuleEntry _ symbol transitions _) -> map (\(currentSymbol, value) -> if currentSymbol == symbol then (currentSymbol, value + transitions) else (currentSymbol, value))) (map (\s -> ([s], 0)) ['A'..'Z'])


addPatternToTotals :: [(String, Int)] -> String -> [(String, Int)]
addPatternToTotals = 
    foldr (\s -> map (\(k,v) -> if k == [s] then (k, v+1) else (k, v)))

solution = (\l -> last l - head l) . sort . map snd . filter (\(k,v) -> v /= 0)

main :: IO ()
main = do
    handle <- openFile "./day14" ReadMode
    contents <- hGetContents handle

    let input = createRuleMap $ words contents
    let initialRuleMap = foldl (addPairTimes 1) input (toPairs pattern)

    print $ solution $ addPatternToTotals (getTotals $ foldr ($) initialRuleMap (replicate 40 step)) pattern
    hClose handle
