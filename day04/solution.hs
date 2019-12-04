import Common
import Data.List
import Data.Char (digitToInt)

import Control.Exception
import Data.Time

range = [146810..612564]

digits :: Int -> [Int]
digits = map digitToInt . show

monotone :: Int -> Bool
monotone password = let passwordDigits = digits password in (sort passwordDigits) == passwordDigits

valid :: Int -> Bool
valid password = let passwordDigits = digits password in 
    monotone password && (length (group passwordDigits) < length passwordDigits)

solution1 :: Int
solution1 = length (filter valid range)

newValid :: Int -> Bool
newValid password = monotone password && any (\g -> (length g) == 2) (group $ digits password)

solution2 :: Int
solution2 = length (filter newValid range)

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