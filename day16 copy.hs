{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import System.IO
import Control.Monad
import Data.Maybe(fromJust)
import Data.Char (digitToInt)

type Bits a = [a]
data PacketType = Literal | Operator deriving Eq
data Packet a = PacketL a (Bits Int) | PacketO { v :: a, payload:: [Packet a], lt :: Int, num :: Int }

instance (Show a) => Show (Packet a) where
    show (PacketL v payload) = "Literal (v: " ++ show v ++ ") " ++ "value: " ++ show (toDecimal payload) ++ " "
    show (PacketO v payload lt nm) = "Operator v: " ++ show v  ++ " lt: " ++ show lt ++ " " ++ " num: " ++ show nm ++ " " ++ " p: (\n" ++ show payload ++ " )"

instance Functor Packet where
    fmap f (PacketL v payload) = PacketL (f v) payload
    fmap f (PacketO v payload lt nm)  = PacketO (f v) (map (fmap f) payload) lt nm

sumTypeForPacket :: Int -> Packet Int -> Int
sumTypeForPacket acc (PacketL v _) = acc + v
sumTypeForPacket acc (PacketO v payload _ _) = sum (map (sumTypeForPacket (acc + v)) payload)


encodeTable :: [(String, String)]
encodeTable = [("0","0000"), ("1","0001"), ("2","0010"), ("3","0011"), ("4","0100"), ("5","0101"), ("6","0110"), ("7","0111"), ("8","1000"), ("9","1001"), ("A","1010"), ("B","1011"), ("C","1100"), ("D","1101"), ("E","1110"), ("F","1111")]

toBits :: [(String, String)] -> String -> [Int]
toBits encodeTable = concatMap (map digitToInt . fromJust . flip lookup encodeTable . (:[]))

toDecimal :: Bits Int -> Int
toDecimal = foldr (\(index, v) acc -> acc + v * 2 ^ index) 0 . zip [0..] . reverse

version :: Bits Int -> Int
version = toDecimal . take 3

getLengthVersion :: Bits Int -> Int 
getLengthVersion = toDecimal . take 1 . drop 6

typeId :: Bits Int -> Int
typeId = toDecimal . take 3 . drop 3

getType :: Bits Int -> PacketType
getType bits = if tId == 4 then Literal else Operator
    where tId = typeId bits

getProperSplit :: Int -> Bits Int -> (Int, Bits Int)
getProperSplit 0 bits = 
    let (lenBits, payload) = splitAt 15 bits
        len = toDecimal lenBits
        (packetPayload, remaining) = splitAt len payload
    in (len, remaining)

getProperSplit 1 bits = 
    let (numPacketBits, remaining) = splitAt 11 bits
        num = toDecimal numPacketBits
    in (num, remaining)


decodePackets :: ([Packet Int], Bits Int) -> Int -> ([Packet Int], Bits Int)
decodePackets (packets, []) _ = (packets, [])
decodePackets acc 1 = acc
decodePackets (packets, bits) n
    | t == Literal =
        let (readed, remaining) = readValue payload ([], [])
        in decodePackets (packets ++ [PacketL v readed], remaining) (n - 1)
    | otherwise  =
        let (readed, remaining) = readOperator lt payload
        in decodePackets (packets ++ [PacketO v readed lt num], remaining) (n - 1)
    where
        t = getType bits
        v = version bits
        lt = getLengthVersion bits
        payload = drop 7 bits
        num = fst $ getProperSplit lt payload



readOperator :: Int -> Bits Int -> ([Packet Int], Bits Int)
readOperator _ [] = ([], [])
readOperator 0 rest = decodeType0Payload rest
readOperator 1 rest = decodeType1Payload rest

decodeType0Payload :: Bits Int -> ([Packet Int], Bits Int)
decodeType0Payload bits =
    let (lenBits, payload) = splitAt 15 bits
        (packetPayload, remaining) = splitAt (toDecimal lenBits) payload
    in decodePackets ([], packetPayload) 100

decodeType1Payload :: Bits Int -> ([Packet Int], Bits Int)
decodeType1Payload bits =
    let (numPacketBits, payload) = splitAt 11 bits
        numPackets = toDecimal numPacketBits
    in decodePackets ([], payload) numPackets


readValue:: Bits Int -> (Bits Int, Bits Int) -> (Bits Int, Bits Int)
readValue [] acc = acc
readValue (x:rest) (readed, unreaded)
    | x == 0 =  (readed ++ firstFour, remaining)
    | x == 1 = readValue remaining (readed ++ firstFour, remaining)
    where (bits, remaining) = splitAt 4 rest
          firstFour = take 4 (bits ++ repeat 0)

main = do
    handle <- openFile "./day16-short" ReadMode
    contents <- hGetContents handle
    let bits = toBits encodeTable contents
    print $ concat $ map show bits
    let packet = decodePackets ([], bits) 100
    print $ sum $ map (sumTypeForPacket 0) $ fst packet
    putStrLn $ show $ head $ fst packet
    hClose handle
