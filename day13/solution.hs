import Common
import IntCode

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import Data.List (intercalate, maximum)

import Debug.Trace

type Point = (Int, Int)
type Screen = Map.Map Point Int

empty  = 0
wall   = 1
block  = 2
paddle = 3
ball   = 4

scoreLocation :: Point
scoreLocation = (-1, 0)

drawScreen :: Screen -> String
drawScreen screen = (if length pointList == 0 then "[ Empty screen ]" else intercalate "\n" $ map row [0..maxY]) ++ ("\nScore: " ++ show (Map.findWithDefault (-1) scoreLocation screen))
    where pointList = Map.toList screen
          maxX  = maximum (map (\((x, _), _) -> x) pointList)
          maxY  = maximum (map (\((_, y), _) -> y) pointList)
          row y = map (\x -> drawCell (Map.findWithDefault empty (x, y) screen))  [0..maxX]
          
drawCell :: Int -> Char
drawCell cell
    | cell == empty  = ' '
    | cell == wall   = '█'
    | cell == block  = '▓'
    | cell == paddle = '='
    | cell == ball   = '©'
    | otherwise      = '?'
    
render :: Screen -> [Int] -> Screen
render screen [] = screen
render screen (id:y:x:is) = render (Map.insert (x, y) id screen) is

-- Part 1
countBlocks :: Screen -> Int
countBlocks screen = length (filter (\x -> snd x == block) (Map.toList screen))

-- Part 2
findCell :: [Int] -> Int -> Int -> Int
findCell [] _ def = def
findCell (id:y:x:is) cell def = if id == cell then x else findCell is cell def

hasCell :: [Int] -> Int -> Bool
hasCell [] _ = False
hasCell (id:y:x:is) cell = if id == cell then True else hasCell is cell

playGame :: State -> Screen -> Int -> Int -> Screen
playGame state@(St _ _ _ _ output) screen lastPaddleX lastBallX = if gameOver
    then screen
    else playGame (runUntilBlocked (withIO state [move] [])) (render screen output) paddleX ballX
    where ballX   = findCell output ball lastBallX
          paddleX = findCell output paddle lastPaddleX
          move = ballX - paddleX
          gameOver = countBlocks screen == 0


main = do
    content <- getInputFromCommaList
    let program = parseProgram content
    let initialState = initState [] program
    let (St _ _ _ _ output) = run initialState
    let screen = render Map.empty output
    putStrLn (drawScreen screen)
    putStrLn ("Blocks: " ++ show (countBlocks screen))
    
    putStrLn "\n------ Part 2 ---------"
    
    let initialPlayState@(St _ _ _ _ initialPlayScreenInstructions) = runUntilBlocked (alterMemory initialState 0 2)
    let initialPlayScreen = (render Map.empty initialPlayScreenInstructions)
    let winScreen = playGame initialPlayState initialPlayScreen 0 0
    putStrLn (drawScreen winScreen)
    
-- Solution 1: 216
-- Solution 2: 10025
