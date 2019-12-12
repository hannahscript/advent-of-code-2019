import Debug.Trace
import Data.List (foldl')
import Data.Time

type Vec3 = (Int, Int, Int)

add :: Vec3 -> Vec3 -> Vec3
add (x, y, z) (x', y', z') = (x + x', y + y', z + z')

-- Position, Velocity
type Body = (Vec3, Vec3)

type System = [Body]

move :: Body -> Body
move (pos, vel) = (add pos vel, vel)

applyBodyGravity :: Body -> Body -> Body
applyBodyGravity (pos@(x, y, z), (vx, vy, vz)) ((x', y', z'), _) = (pos, (vx + (accel $ x `compare` x'), vy + (accel $ y `compare` y'), vz + (accel $ z `compare` z')))
    where accel LT = 1
          accel GT = -1
          accel EQ = 0

applySystemGravity :: System -> Body -> Body
applySystemGravity system body = foldl' applyBodyGravity body system

totalEnergy :: System -> Int
totalEnergy system = sum (map (\((x, y, z), (vx, vy, vz)) -> (abs x + abs y + abs z) * (abs vx + abs vy + abs vz)) system)

step :: System -> System
step system = map (move . (applySystemGravity system)) system

-- Part 2

sysx = [(-17, 0), (-1, 0), (-19, 0), (-6, 0)]
sysy = [(9, 0), (7, 0), (12, 0), (-6, 0)]
sysz = [(-5, 0), (13, 0), (5, 0), (-4, 0)]
moveX (x, v) = (x + v, v)

applyBodyGravityX (x, v) (x', _) = (x, v + (accel $ x `compare` x'))
    where accel LT = 1
          accel GT = -1
          accel EQ = 0

applySystemGravityX system body = foldl' applyBodyGravityX body system

stepX system = map (moveX . (applySystemGravityX system)) system

findRepetitionX initial current steps
    | initial == current = steps
    | otherwise          = findRepetitionX initial (stepX current) (steps + 1)

main = do
    start <- getCurrentTime
    let system = [((-17, 9, -5), (0, 0, 0)), ((-1, 7, 13), (0, 0, 0)), ((-19, 12, 5), (0, 0, 0)), ((-6, -6, -4), (0, 0, 0))]
    print $ totalEnergy $ iterate step system !! 1000
    let sx = findRepetitionX sysx (stepX sysx) 1
    let sy = findRepetitionX sysy (stepX sysy) 1
    let sz = findRepetitionX sysz (stepX sysz) 1
    print (lcm sx (lcm sy sz))
    end <- getCurrentTime
    print (diffUTCTime end start)
    --print $ factor sx
    --print $ factor sy
    --print $ factor sz
    
-- Solution 1: 8742
-- Solution 2: 325433763467176