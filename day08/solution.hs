import Common
import Data.Char (digitToInt)
import Data.List (minimumBy, zip)

count :: Eq a => a -> [a] -> Int
count e = length . filter (==e)

layerWidth = 25
layerHeight = 6
layerSize = layerWidth * layerHeight

readLayers :: Int -> String -> [[Int]]
readLayers size []    = []
readLayers size image = (map digitToInt (take size image)) : (readLayers size (drop size image))

checksum :: [[Int]] -> Int
checksum layers = (count 1 layer) * (count 2 layer)
    where layer = minimumBy (\a b -> (count 0 a) `compare` (count 0 b)) layers
    
superimpose :: [Int] -> [Int] -> [Int]
superimpose front back = map getPixel (zip front back)
    where getPixel (2, p) = p
          getPixel (1, _) = 1
          getPixel (0, _) = 0

combine :: [[Int]] -> [Int]
combine = foldl1 superimpose

displayImage :: [Int] -> String
displayImage []    = []
displayImage image = (map pixelAsChar (take layerWidth image)) ++ "\n" ++ displayImage (drop layerWidth image)
    where pixelAsChar 1 = '█'
          pixelAsChar _ = ' '

main = do
    content <- getInput
    let layers = readLayers layerSize content
    print $ checksum $ layers
    putStr $ displayImage $ combine $ layers
    
-- Solution 1: 1474
-- Solution 2: JCRCB
--   ██  ██  ███   ██  ███
--    █ █  █ █  █ █  █ █  █
--    █ █    █  █ █    ███
--    █ █    ███  █    █  █
-- █  █ █  █ █ █  █  █ █  █
--  ██   ██  █  █  ██  ███
