import Common
import IntCode

import Data.Char
import qualified Data.Map.Strict as Map
import Data.List (inits, isPrefixOf, intersperse, intercalate)

type Vec2     = (Int, Int)
type PointMap = Map.Map Vec2 Char

getASCII :: State -> [Char]
getASCII state = map chr $ reverse $ output $ runUntilBlocked state

createMap :: [Char] -> PointMap
createMap cs = createMap' 0 0 Map.empty cs 
    where
        createMap' x y pointMap []     = pointMap
        createMap' x y pointMap (c:cs) = if c == '\n'
            then createMap' 0 (y+1) pointMap cs
            else createMap' (x+1) y (Map.insert (x, y) c pointMap) cs
            
isIntersection :: PointMap -> Vec2 -> Bool
isIntersection pointMap (x, y) = all (=='#') $ map (\p -> Map.findWithDefault '.' p pointMap) [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

alignmentParameterSum :: PointMap -> Int
alignmentParameterSum pointMap = Map.foldlWithKey (\s (x, y) _ -> s + x * y) 0 $ Map.filterWithKey (\p c -> c == '#' && isIntersection pointMap p) pointMap

-- Part 2

data Direction = R | L deriving (Show, Eq)
data RobotAction = Turn Direction | Walk Int deriving Eq

instance Show RobotAction where
    show (Turn dir)   = show dir
    show (Walk steps) = show steps

add :: Vec2 -> Vec2 -> Vec2
add (x, y) (x', y') = (x + x', y + y')

turnRight :: Vec2 -> Vec2
turnRight (0, y) = (-y, 0)
turnRight (x, 0) = (0, x)
turnRight _      = error "Not an axis unit vector"

turnLeft :: Vec2 -> Vec2
turnLeft (x, 0) = (0, -x)
turnLeft (0, y) = (y, 0)
turnLeft _      = error "Not an axis unit vector"

isScaffolding :: PointMap -> Vec2 -> Bool
isScaffolding pointMap p = (Map.findWithDefault '.' p pointMap) == '#'

getNextDirection :: PointMap -> Vec2 ->  Vec2 -> Maybe (Direction, Vec2)
getNextDirection pointMap pos dir = if rightHasScaffolding then Just (R, rightDir) else (if leftHasScaffolding then Just (L, leftDir) else Nothing)
    where leftDir  = turnLeft dir
          rightDir = turnRight dir
          rightHasScaffolding = isScaffolding pointMap (add pos rightDir)
          leftHasScaffolding  = isScaffolding pointMap (add pos leftDir)
          
travel :: PointMap -> Vec2 -> Vec2 -> Int -> [RobotAction]
travel pointMap pos dir steps = if isScaffolding pointMap (add pos dir)
    then travel pointMap (add pos dir) dir (steps + 1)
    else case getNextDirection pointMap pos dir of
        Nothing  -> [Walk steps]
        Just (nextDir, nextDirV) -> (Walk steps) : (Turn nextDir) : travel pointMap pos nextDirV 0
        
measure :: [RobotAction] -> Int
measure actions = (length $ show actions) - 2

sliding :: [a] -> Int -> [[a]]
sliding _ 0  = []
sliding xs n
    | length xs < n = []
    | otherwise     = (take n xs) : sliding (tail xs) n

consecutiveSubsequences :: [a] -> [[a]]
consecutiveSubsequences list = concat $ map (sliding list) [0..length list]

getSequenceCandidates :: [RobotAction] -> [[RobotAction]]
getSequenceCandidates actions = filter (\s -> (measure s < 20) && (length s > 4)) actionSeqs
    where actionSeqs = consecutiveSubsequences actions
    
findFirstSequence actions = last (filter (\s -> measure s < 20) (inits actions))

splitBySequence seq list = sbs list seq []
    where
        sbs [] s acc = acc : []
        sbs l@(x:xs) s acc
            | isPrefixOf s l = acc : sbs (drop (length s) l) s []
            | otherwise      = sbs xs s (acc ++ [x])
            
headOrEmpty [] = []
headOrEmpty xs = head xs

sequences depth actions = seq' depth [actions] []
    where
        seq' d seqList acc
            | all (== []) seqList = (if d == 0 then acc else [])
            | otherwise           = let candidates = getSequenceCandidates $ head (dropWhile (== []) seqList) in
                                        headOrEmpty $ filter (/= []) $ map (\c -> seq' (d - 1) (concatMap (splitBySequence c) seqList) (c:acc)) candidates
        
findWithIndex :: [a] -> (a -> Bool) -> (Int, a)
findWithIndex list p = fwi list 0
    where
        fwi [] _     = error "Element not found"
        fwi (x:xs) i = if p x then (i, x) else fwi xs (i + 1)
        
fit :: [RobotAction] -> [[RobotAction]] -> [Int]
fit actions sequences = fit' actions
    where
        fit' []   = []
        fit' rest = let (i, sq) = findWithIndex sequences (\s -> isPrefixOf s rest) in i : fit' (drop (length sq) rest)
        
createInput :: [RobotAction] -> String
createInput actions =
    (intersperse ',' (map (\i -> case i of 0 -> 'A'; 1 -> 'B'; 2 -> 'C') seqOrder)) ++ "\n" ++
    (intercalate "\n" (map (\s -> intercalate "," (map show s)) moveSequences)) ++ "\nn\n"
    
    where moveSequences = sequences 3 actions
          seqOrder      = fit actions (moveSequences)
            
vacuum :: State -> String -> Int
vacuum state inputStr = head $ output $ run $ (withInput (alterMemory state 0 2) (map ord inputStr))

main = do
    content <- getInputFromCommaList
    let program = initState [] (parseProgram content)
    let view = getASCII program
    let pointMap = createMap view
    putStrLn ("Part 1: " ++ show (alignmentParameterSum pointMap))
    let initialPos = fst $ head $ filter (\(_, c) -> c == '^') (Map.toList pointMap)
    let actions = tail $ travel pointMap initialPos (0, -1) 0 -- Remove walking 0 steps at beginning
    putStrLn ("Part 2: " ++ show (vacuum program (createInput actions)))
    
-- Solution 1: 5788
-- Solution 2: 648545
