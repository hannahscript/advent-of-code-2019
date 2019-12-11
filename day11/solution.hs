import Common
import IntCode

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (minimum, maximum, intercalate)

import Debug.Trace
import Control.Exception
import Data.Time

-- x, y
type Vector = (Int, Int)

add :: Vector -> Vector -> Vector
add (x, y) (x', y') = (x + x', y + y')

-- Colours for each point
type Hull = Map.Map Vector Int

-- Points that have been painted
type PaintLocations = Set.Set Vector

-- Position, direction
type RobotState = (Vector, Vector)

type PaintProgress = (RobotState, Hull, PaintLocations)

robotCamera :: PaintProgress -> Int
robotCamera ((pos, _), hull, _) = Map.findWithDefault black pos hull

black = 0
white = 1

move :: RobotState -> RobotState
move (pos, dir) = (add pos dir, dir)

-- 0 turn left 90°
-- 1 turn right 90°
turn :: Int -> RobotState -> RobotState
turn 0 (pos, (x, y)) = if x == 0 then (pos, (y, x)) else (pos, (y, -x))
turn 1 (pos, (x, y)) = if x == 0 then (pos, (-y, x)) else (pos, (y, x))

paint :: PaintProgress -> [Int] -> PaintProgress
paint progress [] = progress
paint (robot@(pos, dir), hull, paintLocations) (turnDir:color:is) = paint (move $ turn turnDir robot, Map.insert pos color hull, Set.insert pos paintLocations) is

instructRobot :: PaintProgress -> State -> PaintProgress
instructRobot progress program = case output
    of []               -> progress
       instructions     -> instructRobot (paint progress instructions) newProgram
    where newProgram@(St _ _ _ _ output) = runUntilBlocked (withOutput (withInput program [robotCamera progress]) [])

draw :: Hull -> String
draw hull = intercalate "\n" $ map row (reverse [minY..maxX])
    where pointList = Map.toList hull
          xs    = (map (\((x, _), _) -> x)) pointList
          ys    = (map (\((_, y), _) -> y)) pointList
          minX  = minimum xs
          maxX  = maximum xs
          minY  = minimum ys
          maxY  = maximum ys
          row y = map (\x -> if (Map.findWithDefault black (x, y) hull) == black then ' ' else '█') (reverse [minX..maxX])

main = do
    start <- getCurrentTime
    content <- getInputFromCommaList
    let program = parseProgram content
    -- Solution 1
    let (_, _, paintLocations) = instructRobot (((0, 0), (0, 1)), Map.empty, Set.empty) (initState [] program)
    print $ Set.size paintLocations
    -- Solution 2
    let (_, hull, _) = instructRobot (((0, 0), (0, 1)), Map.fromList [((0, 0), white)], Set.empty) (initState [] program)
    putStrLn $ draw hull
    end <- getCurrentTime
    print (diffUTCTime end start)
    
-- Solution 1: 1967
-- Solution 2: KBUEGZBK
-- █  █ ███  █  █ ████  ██  ████ ███  █  █
-- █ █  █  █ █  █ █    █  █    █ █  █ █ █
-- ██   ███  █  █ ███  █      █  ███  ██
-- █ █  █  █ █  █ █    █ ██  █   █  █ █ █
-- █ █  █  █ █  █ █    █  █ █    █  █ █ █
-- █  █ ███   ██  ████  ███ ████ ███  █  █