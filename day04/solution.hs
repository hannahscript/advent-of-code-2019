import Common
import Data.List
import Data.Char (digitToInt)

import Control.Exception
import Data.Time

range = [146810..612564]

type IntString = [Int]
toIntString :: Int -> IntString
toIntString n = map digitToInt (show n)

monotone :: Int -> Int -> [IntString]
monotone 1 startDigit = [[startDigit]]
monotone digitLength startDigit = let tails = concatMap (\d -> monotone (digitLength - 1) d) [startDigit..9] in
    map (startDigit:) tails
    
monotonesOfLength :: Int -> [IntString]
monotonesOfLength len = (concatMap (monotone len) [1..9])

valid :: IntString -> Bool
valid password = length (group password) < length password

passwordCandidates :: [IntString]
passwordCandidates = let 
    lower = toIntString 146810
    upper = toIntString 612564
    in filter (\p -> p >= lower && p <= upper) (monotonesOfLength 6)

solution1 :: Int
solution1 = length (filter valid passwordCandidates)

newValid :: IntString -> Bool
newValid password = any (\g -> (length g) == 2) (group $ password)

solution2 :: Int
solution2 = length (filter newValid passwordCandidates)

main = do
    start <- getCurrentTime
    putStrLn "Solution 1: "
    print solution1
    putStrLn "Solution 2: "
    print solution2
    end <- getCurrentTime
    print (diffUTCTime end start)
    
-- Solution 1: 1748
-- Solution 2: 1180