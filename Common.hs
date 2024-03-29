module Common (
    getInputAsLines,
    splitBy,
    getInputFromCommaList,
    mapWithIndex,
    stringToInt,
    stringToInteger,
    check,
    getInput
) where

getInput :: IO String
getInput = readFile "input.txt"

getInputAsLines :: IO [String]
getInputAsLines = do
    content <- readFile "input.txt"
    return (lines content)
    
splitBy :: String -> Char -> [String]
splitBy [] _ = []
splitBy str c = token : (splitBy (drop (length token + 1) str) c)
    where token = takeWhile (/=c) str
    
getInputFromCommaList :: IO [String]
getInputFromCommaList = do
    content <- readFile "input.txt"
    return (splitBy content ',')
    
mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f xs = map (\pair -> (f (snd pair) (fst pair))) (zip [0..] xs)

stringToInt :: String -> Int
stringToInt = read

stringToInteger :: String -> Integer
stringToInteger = read

check :: (Eq a, Show a) => String -> a -> a -> String
check description expected actual = description ++ (if expected == actual then " [OK]" else (" failed. Expected " ++ (show expected) ++ ", actual " ++ (show actual)))
