import Common

import Data.List (cycle, foldl')
import Data.Char (digitToInt)
import Data.Array

import Debug.Trace

toDigits :: String -> [Int]
toDigits = map digitToInt

pattern :: Int -> [Int]
pattern n = tail (cycle base)
    where  m = n + 1
           base = (replicate m 0) ++ (replicate m 1) ++ (replicate m 0) ++ (replicate m (-1))

           
phaseStep :: [Int] -> Int -> Int
phaseStep digits n = (abs summed) `mod` 10
    where summed = sum $ zipWith (combine) digits (pattern n)
    
combine :: Int -> Int -> Int
combine d 0    = 0
combine d 1    = d
combine d (-1) = -d
    
phase :: [Int] -> [Int]
phase digits = map (phaseStep digits) [0..(length digits - 1)]
    
fft :: [Int] -> Int -> [Int]
fft digits n = iterate phase digits !! n

-- Part 2

cumsum :: [Int] -> [Int]
cumsum [] = []
cumsum lst = cumsum' 0 lst
    where cumsum' total [] = []
          cumsum' total (x:xs) = (x + total) : cumsum' (total + x) xs
          
digitize :: [Int] -> [Int]
digitize = map (`mod` 10)
    
main = do
    content <- getInput
    print $ take 8 $ fft (toDigits content) 100
    
    let offset = read (take 7 content) :: Int
    let bigInput = reverse $ drop offset $ take (10000 * length content) (cycle $ toDigits content)
    let message = take 8 $ reverse $ iterate (digitize . cumsum) bigInput !! 100
    print message
    
    
-- Solution 1: 29956495
-- Solution 2: 73556504