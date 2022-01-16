{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import System.IO
import Control.Monad
import Data.Maybe(fromJust)
import Data.Char (digitToInt)

type Bits = [Int]
data Step = ParseVersion | ParseType | ParseLengthId | ParseSubpacketLength | ParseNumOfPackets | ParseValue | End deriving (Show, Eq)

data State = State {
    readed :: [(Step , Bits, Int)],
    remaining :: Bits,
    counters :: [StateCounter],
    currentStep :: Step } deriving Show

data StateCounter = StateCounter {
    packetsCount :: Int,
    packetLength :: Int
} deriving Show


-- helpers
encodeTable :: [(String, String)]
encodeTable = [("0","0000"), ("1","0001"), ("2","0010"), ("3","0011"), ("4","0100"), ("5","0101"), ("6","0110"), ("7","0111"), ("8","1000"), ("9","1001"), ("A","1010"), ("B","1011"), ("C","1100"), ("D","1101"), ("E","1110"), ("F","1111")]

toBits :: [(String, String)] -> String -> [Int]
toBits encodeTable = concatMap (map digitToInt . fromJust . flip lookup encodeTable . (:[]))

toDecimal :: Bits -> Int
toDecimal = foldr (\(index, v) acc -> acc + v * 2 ^ index) 0 . zip [0..] . reverse


getNextChunkSize :: Step -> Int
getNextChunkSize ParseVersion = 3
getNextChunkSize ParseType = 3
getNextChunkSize ParseLengthId = 1
getNextChunkSize ParseSubpacketLength = 15
getNextChunkSize ParseNumOfPackets = 11
getNextChunkSize ParseValue = 5

getNextChunkSizeWithCounters :: [StateCounter] -> Step -> Int
getNextChunkSizeWithCounters ((StateCounter _ l):rest) ParseValue = minimum [l, 5]
getNextChunkSizeWithCounters _ t = getNextChunkSize t

examples =
    [
        "D2FE28",
        "38006F45291200",
        "EE00D40C823060",
        "8A004A801A8002F478",
        "620080001611562C8802118E34",
        "C0015000016115A2E0802F182340",
        "A0016C880162017C3686B18A3D4780"
    ]

addLength :: Int ->  [StateCounter] -> [StateCounter]
addLength newLength = (StateCounter 0 newLength :)

addPacketCount :: Int ->  [StateCounter] -> [StateCounter]
addPacketCount newPC  = (StateCounter newPC 0 :)

minusPacket :: [StateCounter] -> [StateCounter]
minusPacket [] = []
minusPacket ((StateCounter pc l):rest)
    | pc == 1 = rest
    | otherwise  =  StateCounter (pc - 1) l:rest

minusLength :: Int -> [StateCounter] -> [StateCounter]
minusLength l [] = []
minusLength newL ((StateCounter pc 0):rest) = rest
minusLength newL ((StateCounter pc l):rest) = StateCounter pc (l - newL):rest

transition :: State  -> State
transition (State readed bits currentCounters currentStep)
    | null bits = st counters End
    | currentStep == ParseVersion = st (minusPacket counters) ParseType
    | currentStep == ParseType = if stateValue == 4 then st counters ParseValue else st counters ParseLengthId
    | currentStep == ParseLengthId =
        if stateValue == 0 then st (addLength stateValue counters) ParseSubpacketLength
        else st (addPacketCount stateValue counters) ParseNumOfPackets
    | currentStep == ParseSubpacketLength = st counters ParseVersion
    | currentStep == ParseNumOfPackets = st counters ParseVersion
    | currentStep == ParseValue = if head stateBits == 1 then st counters ParseValue else st counters ParseVersion
    where
        chunkSize = getNextChunkSizeWithCounters currentCounters currentStep
        (stateBits, remaining) = splitAt chunkSize bits
        stateValue = toDecimal stateBits
        st = State ((currentStep, stateBits, stateValue):readed) remaining
        counters = minusLength chunkSize currentCounters


decodeLoop :: State -> State
decodeLoop (State readed bits counter End) = State readed bits counter End
decodeLoop state = decodeLoop (transition state)

getTypeSum :: State -> Int
getTypeSum = sum . map (\(_, _, v) -> v) . filter (\(t, _, _) -> t == ParseVersion) . readed


main = do
    handle <- openFile "./day16" ReadMode
    contents <- hGetContents handle
    -- let bits = toBits encodeTable (examples!!6)
    let bits = toBits encodeTable contents
    let state = State [] bits [] ParseVersion
    print bits
    print $ decodeLoop state
    -- print $ foldr (\_ acc -> acc . transition ) id [0..8] state
    print "sum is ---->"
    print $ getTypeSum $ decodeLoop state
    hClose handle