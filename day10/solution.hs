import Common
import qualified Data.IntMap.Strict as IntMap
import Data.List (zipWith, lines, zip, minimumBy, maximumBy, sortBy)

import Debug.Trace
import Control.Exception
import Data.Time

-- x, y
type Point = (Int, Int)

-- If true, contains an asteroid
-- Map, Width, Height
type StarMap = (IntMap.IntMap Bool, Int, Int)

pointTo1DCoord :: Point -> Int -> Int
pointTo1DCoord (x, y) height = y * height + x

hasAsteroid :: StarMap -> Point -> Bool
hasAsteroid (starMap, _, height) p = IntMap.findWithDefault False (pointTo1DCoord p height) starMap

euclid :: Int -> Int -> Int
euclid a 0 = a
euclid a b = euclid b (a `mod` b)

scale :: Int -> Point -> Point
scale a (x, y) = (a * x, a * y)

divp :: Int -> Point -> Point
divp a (x, y) = (x `div` a, y `div` a)

add :: Point -> Point -> Point
add (x, y) (x', y') = (x + x', y + y')

substract :: Point -> Point -> Point
substract (x, y) (x', y') = (x - x', y - y')

pointsBetween :: Point -> Point -> [Point]
pointsBetween a b = if d == 1
    then [a, b]
    else zipWith (\n v -> add a (scale n v)) [0..d] (repeat ndir)
    where dir@(dx, dy) = substract b a
          d            = abs $ euclid dx dy
          ndir         = divp d dir
          
canSee :: StarMap -> Point -> Point -> Bool
canSee starMap a b = length (filter (hasAsteroid starMap) (pointsBetween a b)) == 2

createStarmap :: String -> StarMap
createStarmap repr = (IntMap.fromList (concat point2DList), width, height)
    where rows   = lines repr
          height = length rows
          width = length (rows !! 0)
          point2DList = mapWithIndex (\row y -> (mapWithIndex (\p x -> (y * height + x, p == '#')) row)) rows
          
allPoints :: StarMap -> [Point]
allPoints (_, w, h) = [(x, y) | x <- [0..w-1], y <- [0..h-1]]

getMostVisible :: StarMap -> (Point, Int)
getMostVisible starMap = maximumBy (\(_, a) (_, b) -> a `compare` b) (map (\p -> (p, length (filter (\q -> p /= q && canSee starMap p q) points))) points)
    where points = filter (hasAsteroid starMap) (allPoints starMap)
          
-- Part 2

laserPoints :: StarMap -> Point -> [Point]
laserPoints starMap origin = cycle $ (sortBy (\p q -> (angle origin p) `compare` (angle origin q)) visibleAsteroids)
    where asteroids        = filter (hasAsteroid starMap) (allPoints starMap)
          visibleAsteroids = filter (\p -> p /= origin && canSee starMap origin p) asteroids

angle :: Point -> Point -> Double
angle origin p = (if baseAngle < 0 then 2*pi + baseAngle else baseAngle)
    where p'@(x, y) = substract p origin
          baseAngle = atan2 (fromIntegral (x)) (fromIntegral (-y))

        
dist2 :: Point -> Point -> Int
dist2 (x, y) (x', y') = dx * dx + dy * dy
    where dx = x' - x
          dy = y' - y
        
getClosestAsteroidBetween :: StarMap -> Point -> Point -> Maybe Point
getClosestAsteroidBetween starMap origin target = if length asteroidsInSight > 0
        then Just $ minimumBy (\p q -> (dist2 p origin) `compare` (dist2 q origin)) asteroidsInSight
        else Nothing
    where asteroidsInSight = filter (\p -> p /= origin && hasAsteroid starMap p) (pointsBetween origin target)
    
destroyAsteroid :: StarMap -> Point -> StarMap
destroyAsteroid (starMap, width, height) p = (IntMap.insert (pointTo1DCoord p height) False starMap, width, height)
    
vaporize :: StarMap -> Point -> [Point] -> [Point]
vaporize starMap origin (p:ps) = case (getClosestAsteroidBetween starMap origin p)
    of Just hit -> hit : (vaporize (destroyAsteroid starMap hit) origin ps)
       Nothing  -> (vaporize starMap origin ps)

fireLaser :: StarMap -> Point -> [Point]
fireLaser starMap@(_, width, height) origin@(x, y) = vaporize starMap origin targetPoints
    where targetPoints = laserPoints starMap origin

main = do
    start <- getCurrentTime
    input <- getInput
    let starMap = createStarmap input
    let (monitoringStation, visibleAsteroids) = getMostVisible starMap
    print $ ((show visibleAsteroids) ++ " visible at " ++ (show monitoringStation))
    print $ (take 200 $ fireLaser starMap monitoringStation)
    end <- getCurrentTime
    print $ (diffUTCTime end start)
    
-- Solution 1: 340
-- Solution 2: 2628