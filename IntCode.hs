module IntCode (
    State (St),
    parseProgram,
    initState,
    run,
    advance
) where

import Common
import Data.Char (digitToInt)
import Debug.Trace

-- Instruction pointer, Input, Memory, Output
data State = St Int [Int] [Int] [Int]

-- Actual opcode, parameters
data OpCode = Code Int [Int]

instance Show OpCode where
    show (Code code params) = "#" ++ (Prelude.show code) ++ " [" ++ (Prelude.show params) ++ "]"
    
instance Show State where
    show (St ip input memory output) = "IP#" ++ (Prelude.show ip) ++ " IN=" ++ (Prelude.show input) ++ " OUT=" ++ (Prelude.show output)

getDefaultModes :: Int -> [Int]
getDefaultModes 1 = [0, 0, 1] -- add
getDefaultModes 2 = [0, 0, 1] -- mult
getDefaultModes 3 = [1]       -- read
getDefaultModes 4 = [0]       -- write
getDefaultModes 5 = [0, 0]    -- jumpiftrue
getDefaultModes 6 = [0, 0]    -- jumpiffalse
getDefaultModes 7 = [0, 0, 1] -- lessthan
getDefaultModes 8 = [0, 0, 1] -- equals
getDefaultModes 99 = []       -- end
    
addDefaults :: [Int] -> [Int] -> [Int]
addDefaults defaultList list = list ++ defaultTail
    where defaultTail = drop (length list) defaultList

getParams :: State -> [Int] -> [Int]
getParams (St ip input memory output) paramModes = mapWithIndex (\mode offset -> getParam ip offset mode) paramModes
    where getParam base offset 0 = memory !! (memory !! (base + offset + 1)) -- position mode
          getParam base offset 1 = memory !! (base + offset + 1)             -- immediate mode

parseOpCode :: State -> OpCode
parseOpCode state@(St ip input memory output) = Code code params
    where num = memory !! ip
          (codeR : codeL : paramModes) = reverse (let n = show num in if length n == 1 then '0':n else n)
          code = stringToInt (codeL:codeR:[])
          params = getParams state (addDefaults (getDefaultModes code) (map digitToInt paramModes))
          
parseProgram :: [String] -> [Int]
parseProgram program = map stringToInt program

replaceInList :: [a] -> a -> Int -> [a]
replaceInList [] _ _ = []
replaceInList (x:xs) e i = if i > 0 then x : (replaceInList xs e (i - 1)) else e : xs

instruction_add :: State -> OpCode -> State
instruction_add state@(St ip input memory output) opcode@(Code code params) = St (ip + 4) input (replaceInList memory (a + b) replaceIndex) output
    where [a, b, replaceIndex] = params
          
instruction_mult :: State -> OpCode -> State
instruction_mult state@(St ip input memory output) opcode@(Code code params) = St (ip + 4) input (replaceInList memory (a * b) replaceIndex) output
    where [a, b, replaceIndex] = params
          
instruction_read :: State -> OpCode -> State
instruction_read state@(St ip [] memory output) opcode@(Code code params) = state -- If input is empty, wait
instruction_read state@(St ip input memory output) opcode@(Code code params) = St (ip + 2) restInput (replaceInList memory i replaceIndex) output
    where [replaceIndex] = params
          (i:restInput) = input
          
instruction_write :: State -> OpCode -> State
instruction_write state@(St ip input memory output) opcode@(Code code params) = St (ip + 2) input memory (out:output)
    where [out] = params
    
instruction_jumpiftrue :: State -> OpCode -> State
instruction_jumpiftrue state@(St ip input memory output) opcode@(Code code params) = St (if test /= 0 then val else ip + 3) input memory output
    where [test, val] = params
    
instruction_jumpiffalse :: State -> OpCode -> State
instruction_jumpiffalse state@(St ip input memory output) opcode@(Code code params) = St (if test == 0 then val else ip + 3) input memory output
    where [test, val] = params
    
instruction_lessthan :: State -> OpCode -> State
instruction_lessthan state@(St ip input memory output) opcode@(Code code params) = St (ip + 4) input (replaceInList memory (if a < b then 1 else 0) replaceIndex) output
    where [a, b, replaceIndex] = params
    
instruction_equals :: State -> OpCode -> State
instruction_equals state@(St ip input memory output) opcode@(Code code params) = St (ip + 4) input (replaceInList memory (if a == b then 1 else 0) replaceIndex) output
    where [a, b, replaceIndex] = params

dispatch :: State -> OpCode -> State
dispatch state opcode@(Code 1 _) = instruction_add state opcode
dispatch state opcode@(Code 2 _) = instruction_mult state opcode
dispatch state opcode@(Code 3 _) = instruction_read state opcode
dispatch state opcode@(Code 4 _) = instruction_write state opcode
dispatch state opcode@(Code 5 _) = instruction_jumpiftrue state opcode
dispatch state opcode@(Code 6 _) = instruction_jumpiffalse state opcode
dispatch state opcode@(Code 7 _) = instruction_lessthan state opcode
dispatch state opcode@(Code 8 _) = instruction_equals state opcode

advance :: State -> State
advance empty@(St ip input [] output) = empty
advance state@(St ip input memory output) = let opcode@(Code code _) = parseOpCode state
    in if code == 99 then state else dispatch state opcode

run :: State -> State
run empty@(St ip input [] output) = empty
run state@(St ip input memory output) = let opcode@(Code code _) = parseOpCode state
    in if code == 99 then state else run (dispatch state opcode)
    

    
initState :: [Int] -> [Int] -> State
initState input program = St 0 input program []