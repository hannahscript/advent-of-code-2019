import Common
import Data.List
import Data.Set (fromList, member)

stringToInt :: String -> Int
stringToInt = read

type Vector = (Int, Int)

addVector :: Vector -> Vector -> Vector
addVector (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

scaleVector :: Int -> Vector -> Vector
scaleVector fac (x, y) = (fac * x, fac * y)

manhattan :: Vector -> Int
manhattan (x, y) = (abs x) + (abs y)

fasterIntersect :: Ord a => [a] -> [a] -> [a]
fasterIntersect a b = let collisionSet = fromList a in
    filter (\e -> member e collisionSet) b

directionToVectors :: String -> [Vector]
directionToVectors (dir:lenStr) = take (stringToInt lenStr) (tail (iterate (\v -> addVector v directionVector) (0, 0)))
    where directionVector = case dir of 'U' -> (0, 1)
                                        'R' -> (1, 0)
                                        'D' -> (0, -1)
                                        'L' -> (-1, 0)
                                        otherwise -> (0, 0)

getPathFrom :: Vector -> [String] -> [Vector]
getPathFrom origin [] = []
getPathFrom origin (dir:rest) = let newPoints = (map (\v -> addVector v origin) (directionToVectors dir)) in
    newPoints ++ (getPathFrom (last newPoints) rest)

solution1 :: [String] -> [String] -> Int
solution1 directionsA directionsB = let
    pathA = getPathFrom (0, 0) directionsA
    pathB = getPathFrom (0, 0) directionsB
    intersections = fasterIntersect pathB pathA
    closestIntersection = (sortOn manhattan intersections) !! 0
    in manhattan closestIntersection
    
data AnnotatedVector = AVec Vector Int
mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f xs = map (\pair -> (f (snd pair) (fst pair))) (zip [0..] xs)

toStepVectors :: Int -> [Vector] -> [AnnotatedVector]
toStepVectors baseSteps = mapWithIndex (\v i -> AVec v (i + baseSteps + 1))

fromStepVector :: AnnotatedVector -> Vector
fromStepVector (AVec v _) = v

instance Show AnnotatedVector where
    show (AVec v steps) = (Prelude.show v) ++ " | " ++ (Prelude.show steps)

getPathFromWithSteps :: AnnotatedVector -> Int -> [String] -> [AnnotatedVector]
getPathFromWithSteps origin steps [] = []
getPathFromWithSteps origin steps (dir:rest) = let newPoints = toStepVectors steps (map (\v -> addVector v (fromStepVector origin)) (directionToVectors dir)) in
    newPoints ++ (getPathFromWithSteps (last newPoints) (steps + (length newPoints)) rest)
    
findStepVector :: Vector -> [AnnotatedVector] -> AnnotatedVector
findStepVector v stepVectors = head (filter (\(AVec w s) -> v == w) stepVectors)

addSteps :: AnnotatedVector -> AnnotatedVector -> Int
addSteps (AVec _ s) (AVec _ t) = s + t
    
solution2 :: [String] -> [String] -> Int
solution2 directionsA directionsB = let
    pathA = getPathFrom (0, 0) directionsA
    pathB = getPathFrom (0, 0) directionsB
    intersections = fasterIntersect pathB pathA
    pathAWithSteps = getPathFromWithSteps (AVec (0, 0) 0) 0 directionsA
    pathBWithSteps = getPathFromWithSteps (AVec (0, 0) 0) 0 directionsB
    in head $ sort $ map (\v -> addSteps (findStepVector v pathAWithSteps) (findStepVector v pathBWithSteps)) intersections

main = do
    [directionsStrA, directionsStrB] <- getInputAsLines
    let directionsA = (splitBy directionsStrA ',')
    let directionsB = (splitBy directionsStrB ',')
    let result1 = solution1 directionsA directionsB
    print result1
    let result2 = solution2 directionsA directionsB
    print result2