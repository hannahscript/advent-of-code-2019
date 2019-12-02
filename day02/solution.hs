import Common
import Control.Exception


stringToInt :: String -> Int
stringToInt = read

replaceInList :: [a] -> a -> Int -> [a]
replaceInList [] _ _ = []
replaceInList (x:xs) e i = if i > 0 then x : (replaceInList xs e (i - 1)) else e : xs

opCode1 :: [Int] -> Int -> [Int]
opCode1 state ip = replaceInList state (value1 + value2) replaceIndex
    where value1 = state !! (state !! (ip + 1))
          value2 = state !! (state !! (ip + 2))
          replaceIndex = state !! (ip + 3)
          
opCode2 :: [Int] -> Int -> [Int]
opCode2 state ip = replaceInList state (value1 * value2) replaceIndex
    where value1 = state !! (state !! (ip + 1))
          value2 = state !! (state !! (ip + 2))
          replaceIndex = state !! (ip + 3)
          
intcodeMachine :: [Int] -> Int -> [Int]
intcodeMachine [] _ = []
intcodeMachine state ip
    | instruction == 1 = intcodeMachine (opCode1 state ip) (ip + 4)
    | instruction == 2 = intcodeMachine (opCode2 state ip) (ip + 4)
    | instruction == 99 = state
    | otherwise = state
    where instruction = state !! ip
    
preprocess :: [Int] -> Int -> Int -> [Int]
preprocess state noun verb = replaceInList (replaceInList state noun 1) verb 2

solution1 :: [Int] -> Int
solution1 state = head (intcodeMachine (preprocess state 12 2) 0)

solution2 :: [Int] -> Int
solution2 state = 100 * noun + verb
    where (noun, verb) = head  [(noun, verb) | noun <- [0..99], verb <- [0..99], (head (intcodeMachine (preprocess state noun verb) 0)) == 19690720]

--- Apply solution ---

main = do
    contentParts <- getInputFromCommaList
    numbers <- return (map stringToInt contentParts)
    putStrLn "Input: "
    print numbers
    let result1 = solution1 numbers
    putStrLn "Solution 1: "
    print result1
    let totalFuel = solution2 numbers
    putStrLn "Solution 2: "
    print totalFuel
    
-- Solution 1: 5534943
-- Solution 2: 7603