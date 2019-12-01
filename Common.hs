module Common (
    getInputAsLines
) where

getInputAsLines :: IO [String]
getInputAsLines = do
    content <- readFile "input.txt"
    return (lines content) 