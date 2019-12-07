import Common
import IntCode

import Data.List
import Debug.Trace

phaseSettings :: [[Int]]
phaseSettings = permutations [0, 1, 2, 3, 4]

findOptimalSettings :: [Int] -> [[Int]] -> Int -> Int
findOptimalSettings program (setting:settings) best = findOptimalSettings program settings (max best attempt)
    where attempt = trySetting program setting 0
findOptimalSettings _ [] best = best

trySetting :: [Int] -> [Int] -> Int -> Int
trySetting program (phase:phases) initial = trySetting program phases phaseOut
    where (St _ _ _ [phaseOut]) = run (initState [phase, initial] program)
trySetting program [] initial = initial

getPreviousState :: [State] -> Int -> State
getPreviousState states fromIndex = states !! index
    where index = let tryIndex = fromIndex - 1 in if tryIndex < 0 then length states - 1 else tryIndex

pipeIO :: [State] -> [State]
pipeIO states = map (\(St ip input memory output) -> (St ip input memory [])) (mapWithIndex pipePreviousOutput states)
    where pipePreviousOutput (St ip input memory output) i = let (St _ _ _ previousOutput) = getPreviousState states i in (St ip (previousOutput ++ input) memory output)

runSerial :: [State] -> [Int] -> [State]
runSerial states ips
    | ips == newIps = states
    | otherwise = runSerial newStates newIps
    where newStates = map advance (pipeIO states)
          newIps = map (\(St ip _ _ _) -> ip) newStates
          
parallelPhaseSettings :: [[Int]]
parallelPhaseSettings = permutations [5, 6, 7, 8, 9]

trySettingInLoop :: [Int] -> [Int] -> Int
trySettingInLoop program setting = let (St _ _ _ output) = last endStates in head output
    where endStates = runSerial (firstAmplifier : restAmplifiers) (map (\_->0) setting)
          firstAmplifier = initState [head setting, 0] program
          restAmplifiers = map (\s -> (initState [s] program)) (tail setting)

findOptimalSettingsInLoop :: [Int] -> [[Int]] -> Int -> Int
findOptimalSettingsInLoop program (setting:settings) best = findOptimalSettingsInLoop program settings (max best attempt)
    where attempt = trySettingInLoop program setting
findOptimalSettingsInLoop _ [] best = best

main = do
    content <- getInputFromCommaList
    let program = parseProgram content
    print (findOptimalSettings program phaseSettings 0)
    print (findOptimalSettingsInLoop program parallelPhaseSettings 0)
    
-- Solution 1: 117312
-- Solution 2: 1336480
