module Common (
    getInputAsLines,
    splitBy,
    getInputFromCommaList
) where

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