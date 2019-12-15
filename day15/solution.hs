import Common
import IntCode

import qualified Data.Map.Strict as Map
import Data.List (intercalate, foldl', nub)

import Debug.Trace

type Point = (Int, Int)
type ShipMap = Map.Map Point Int

data Direction = North | South | East | West deriving (Eq, Show)

directionToInt :: Direction -> Int
directionToInt North = 1
directionToInt South = 2
directionToInt West  = 3
directionToInt East  = 4

reverseDir :: Direction -> Direction
reverseDir North = South
reverseDir South = North
reverseDir East  = West
reverseDir West  = East

go :: Point -> Direction -> Point
go (x, y) North = (x, y + 1)
go (x, y) South = (x, y - 1)
go (x, y) East  = (x + 1, y)
go (x, y) West  = (x - 1, y)

neighbours :: Point -> [(Direction, Point)]
neighbours pos = map (\d -> (d, go pos d)) [North, East, South, West]

search :: State -> ShipMap -> Point -> [Direction] -> [Direction]
search state shipMap pos path = case nextNeighbour of
    Nothing          -> (if path == [] then [] else search (runUntilBlocked (withIO state [directionToInt backDir] [])) shipMap (go pos backDir) (tail path))
    Just (dir, nPos) -> (let nextState = runUntilBlocked (withIO state [directionToInt dir] []) in
        case head (output nextState) of
            0 -> search nextState (Map.insert nPos 0 shipMap) pos path
            1 -> search nextState (Map.insert nPos (head (output nextState)) shipMap) nPos (dir:path)
            2 -> dir:path)
    where unvisitedNeighbours = filter (\(_, p) -> not $ Map.member p shipMap) (neighbours pos)
          nextNeighbour       = if length unvisitedNeighbours > 0 then Just $ head unvisitedNeighbours else Nothing
          backDir             = if path == [] then North else reverseDir (head path)                                   -- if path == [] // North is just a meaningless placeholder value that would never be used

-- Part 2

exploreMap :: State -> ShipMap -> Point -> [Direction] -> ShipMap
exploreMap state shipMap pos path = case nextNeighbour of
    Nothing          -> (if path == [] then shipMap else exploreMap (runUntilBlocked (withIO state [directionToInt backDir] [])) shipMap (go pos backDir) (tail path))
    Just (dir, nPos) -> (let nextState = runUntilBlocked (withIO state [directionToInt dir] []) in
        case head (output nextState) of
            0 -> exploreMap nextState (Map.insert nPos 0 shipMap) pos path
            _ -> exploreMap nextState (Map.insert nPos (head (output nextState)) shipMap) nPos (dir:path))
    where unvisitedNeighbours = filter (\(_, p) -> not $ Map.member p shipMap) (neighbours pos)
          nextNeighbour       = if length unvisitedNeighbours > 0 then Just $ head unvisitedNeighbours else Nothing
          backDir             = if path == [] then North else reverseDir (head path)                                   -- if path == [] // North is just a meaningless placeholder value that would never be used

walkPath :: Point -> [Direction] -> Point
walkPath pos path = foldl go pos path

getEmptyNeighbours :: ShipMap -> Point -> [Point]
getEmptyNeighbours shipMap pos = filter isEmpty $ map (go pos) [North, East, South, West]
    where
        isEmpty p = case Map.lookup p shipMap of
            Nothing -> False
            Just 1  -> True
            _       -> False
        
insertMany :: ShipMap -> [(Point, Int)] -> ShipMap
insertMany shipMap kvs = foldl' (\m (k, v) -> Map.insert k v m) shipMap kvs
        
oxygenate :: ShipMap -> Point -> Int
oxygenate shipMap origin = oxygenate' shipMap [origin] 0
    where
        oxygenate' shipMap oxygenatedPoints time = let newPoints = nub $ concatMap (getEmptyNeighbours shipMap) oxygenatedPoints in
            if newPoints == []
            then time
            else oxygenate' (insertMany shipMap (map (\p -> (p, 0)) newPoints)) (oxygenatedPoints ++ newPoints) (time + 1)
            
-- Fun
draw :: ShipMap -> String
draw shipMap = intercalate "\n" $ map row (reverse [minY..maxY])
    where pointList = Map.toList shipMap
          xs    = (map (\((x, _), _) -> x)) pointList
          ys    = (map (\((_, y), _) -> y)) pointList
          minX  = minimum xs
          maxX  = maximum xs
          minY  = minimum ys
          maxY  = maximum ys
          row y = map (\x -> drawCell (Map.findWithDefault 0 (x, y) shipMap)) (reverse [minX..maxX])
          
drawCell :: Int -> Char
drawCell 0 = '█'
drawCell 1 = '.'
drawCell 2 = '@'
drawCell _ = '?'

---

main = do
    content <- getInputFromCommaList
    let program  = initState [] (parseProgram content)
    let pathToOxygen = search program Map.empty (0, 0) []
    putStrLn ("Distance to oxygen " ++ (show $ length pathToOxygen))
    let shipMap = exploreMap program Map.empty (0, 0) []
    let time = oxygenate shipMap (walkPath (0, 0) pathToOxygen)
    putStrLn ("Time to oxygenate " ++ show time)
    putStrLn $ draw shipMap
    
-- Solution 1: 226
-- Solution 2: 342
{- 
█████████████████████████████████████████
█...........█.....█.....█...█...........█
█.███████.███.█.█.███.███.█.█.█████.███.█
█.█...█...█...█.█...█.█...█...█...█...█.█
█.█.███.█.█.███.███.█.█.███████.█.███.█.█
█.█.█...█.█.█...█...█.█...█.....█.█.█.█.█
█.█.█.███.█.█.█████.█.███.█.█████.█.█.█.█
█...█...█.█.█.....█.....█.█.█.....█.█.█.█
███.███.█.█.█████.█.█████.█.█.█████.█.█.█
█.█...█.█.█.█.█...█.█.....█.█.......█.█.█
█.█.███.█.█.█.█.███.█.█████.███████.█.███
█...█...█.█...█...█.█.....█.....█...█...█
█.███.███████.███.███████.███.█.█.█████.█
█...█.█.....█.█...█.....█...█.█.█.█.█...█
█████.█.███.█.█.███.███.███.█.█.█.█.█.█.█
█.....█.█.█...█.......█.....█.█.█.█.█.█.█
█.███.█.█.█████████████.███████.█.█.█.███
█.█...█.█...█.....█...█.........█...█...█
█.█████.█.█.█████.█.█.█████████████████.█
█.....█...█.......█.█...█.....█.....█...█
█.███.███████████.█.███.█.███.█.███.█.█.█
█.█...█...█...█...█.█...█.█.█.█.█.█...█.█
█.█.█.███.█.█.█.█████.███.█.█.█.█.█████.█
█.█.█...█.█.█.......█.....█...█.█.......█
█.█.███.█.█.███████████████.███.█.███████
█.█...█...█.█.............█...█.█...█...█
█.███.█████.█.█████.█████████.█.███.███.█
█...█.█.....█.....█...........█...█...█.█
█.███.█.█████████.███████████████.███.█.█
█.█...█...█.......█...█.....█.....█.....█
███.█.███.█.███████.███.███.█.█.███████.█
█...█...█.█.█.....█.......█...█.....█...█
█.███████.█.███.█.███████.█████████.███.█
█.....█...█...█.█.......█...█.....█...█.█
█.███.█.█████.█.███████.█████.███.███.█.█
█...█.█.█...█.█.....█.█.....█.█...█...█.█
█.███.█.███.█.█.███.█.█████.█.█.███.███.█
█.█...█.....█.█...█.█.█...█.█.█.....█...█
█.█.███████.█.█████.█.█.█.█.█.███████.███
█.█.........█.......█...█.....█@........█
█████████████████████████████████████████
-}