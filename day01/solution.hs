import Common

massToFuel :: Integer -> Integer
massToFuel m = ((m `div` 3) - 2)

stringToInteger :: String -> Integer
stringToInteger str = read str :: Integer

mapAsIntegerAndSum :: (Integer -> Integer) -> [String] -> Integer
mapAsIntegerAndSum f strings = (sum (map (f . stringToInteger) strings))

solution1 :: [String] -> Integer
solution1 = mapAsIntegerAndSum massToFuel

fuelSequence :: Integer -> [Integer]
fuelSequence mass = let
    fuel = massToFuel mass
    in fuel : (fuelSequence fuel)

getTotalFuel :: Integer -> Integer
getTotalFuel mass = (sum (takeWhile (>0) (fuelSequence mass)))
    
solution2 :: [String] -> Integer
solution2 = mapAsIntegerAndSum getTotalFuel

--- Apply solution ---

main = do
    contentLines <- getInputAsLines
    let moduleFuel = solution1 contentLines
    putStr "Solution 1: "
    print moduleFuel
    let totalFuel = solution2 contentLines
    putStr "Solution 2: "
    print totalFuel
    
-- Solution 1: 3390596
-- Solution 2: 5083024