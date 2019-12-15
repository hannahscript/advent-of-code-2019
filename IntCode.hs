module IntCode (
    State (St),
    parseProgram,
    initState,
    run,
    runUntilBlocked,
    advance,
    withInput,
    withOutput,
    withIO,
    alterMemory,
    output
) where

import Common
import Data.Char (digitToInt)
import qualified Data.IntMap.Strict as IntMap
import Debug.Trace

-- Instruction pointer, relativeBase, Input, Memory, Output
data State = St Int Int [Int] (IntMap.IntMap Int) [Int]

-- Actual opcode, parameters
data OpCode = Code Int [Int]

data ParameterMode = Dest | Src deriving Show
type Parameter = (Int, ParameterMode)

instance Show OpCode where
    show (Code code params) = "#" ++ (Prelude.show code) ++ (Prelude.show params)
    
instance Show State where
    show (St ip _ input _ output) = "IP#" ++ (Prelude.show ip) ++ " IN=" ++ (Prelude.show input) ++ " OUT=" ++ (Prelude.show output)

(!) :: (IntMap.IntMap Int) -> Int -> Int
map ! i = IntMap.findWithDefault 0 i map

getParameterTypes :: Int -> [ParameterMode]
getParameterTypes 1    = [Src, Src, Dest] -- add
getParameterTypes 2    = [Src, Src, Dest] -- mult
getParameterTypes 3    = [Dest]           -- read
getParameterTypes 4    = [Src]            -- write
getParameterTypes 5    = [Src, Src]       -- jumpiftrue
getParameterTypes 6    = [Src, Src]       -- jumpiffalse
getParameterTypes 7    = [Src, Src, Dest] -- lessthan
getParameterTypes 8    = [Src, Src, Dest] -- equals
getParameterTypes 9    = [Src]            -- changebase
getParameterTypes 99   = []              -- end
getParameterTypes code = error $ "Unknown opcode " ++ (show code)
    
zipDefaultL :: a -> [a] -> [b] -> [(a,b)]
zipDefaultL d [] []         = []
zipDefaultL d (x:xs) []     = []
zipDefaultL d [] (y:ys)     = (d, y) : zipDefaultL d [] ys
zipDefaultL d (x:xs) (y:ys) = (x, y) : zipDefaultL d xs ys
    
getParameters :: Int -> [Int] -> [Parameter]
getParameters code paramModes = zipDefaultL 0 paramModes (getParameterTypes code)

getParameterValues :: State -> [Parameter] -> [Int]
getParameterValues (St ip base input memory output) paramModes = mapWithIndex (\(mode, ptype) offset -> getParam (offset + 1) mode ptype) paramModes
    where immediateValue offset = memory ! (ip + offset)
          getParam offset 0 Dest = immediateValue offset                     -- position mode, dest
          getParam offset 0 Src  = memory ! (immediateValue offset)          -- position mode, src
          getParam offset 1 _    = immediateValue offset                     -- immediate mode, src (dest will never be immediate)
          getParam offset 2 Dest = base + (immediateValue offset)            -- relative mode, dest
          getParam offset 2 Src  = memory ! (base + (immediateValue offset)) -- relative mode

digitize :: Int -> [Int]
digitize = (map digitToInt) . show

parseOpCode :: State -> OpCode
parseOpCode state@(St ip base input memory output) = Code code params
    where num = memory ! ip
          (codeR : codeL : paramModes) = let digits = digitize num in reverse (if num < 100 then 0:digits else digits)
          code = codeL * 10 + codeR
          params = getParameterValues state (getParameters code paramModes)
          
parseProgram :: [String] -> [Int]
parseProgram program = map stringToInt program

instruction_add :: State -> OpCode -> State
instruction_add state@(St ip base input memory output) opcode@(Code code params) = St (ip + 4) base input (IntMap.insert replaceIndex (a + b) memory) output
    where [a, b, replaceIndex] = params
          
instruction_mult :: State -> OpCode -> State
instruction_mult state@(St ip base input memory output) opcode@(Code code params) = St (ip + 4) base input (IntMap.insert replaceIndex (a * b) memory) output
    where [a, b, replaceIndex] = params
          
instruction_read :: State -> OpCode -> State
instruction_read state@(St ip base [] memory output) opcode@(Code code params) = state -- If input is empty, wait
instruction_read state@(St ip base input memory output) opcode@(Code code params) = St (ip + 2) base restInput (IntMap.insert replaceIndex i memory) output
    where [replaceIndex] = params
          (i:restInput) = input
          
instruction_write :: State -> OpCode -> State
instruction_write state@(St ip base input memory output) opcode@(Code code params) = St (ip + 2) base input memory (out:output)
    where [out] = params
    
instruction_jumpiftrue :: State -> OpCode -> State
instruction_jumpiftrue state@(St ip base input memory output) opcode@(Code code params) = St (if test /= 0 then val else ip + 3) base input memory output
    where [test, val] = params
    
instruction_jumpiffalse :: State -> OpCode -> State
instruction_jumpiffalse state@(St ip base input memory output) opcode@(Code code params) = St (if test == 0 then val else ip + 3) base input memory output
    where [test, val] = params
    
instruction_lessthan :: State -> OpCode -> State
instruction_lessthan state@(St ip base input memory output) opcode@(Code code params) = St (ip + 4) base input (IntMap.insert replaceIndex (if a < b then 1 else 0) memory) output
    where [a, b, replaceIndex] = params
    
instruction_equals :: State -> OpCode -> State
instruction_equals state@(St ip base input memory output) opcode@(Code code params) = St (ip + 4) base input (IntMap.insert replaceIndex (if a == b then 1 else 0) memory) output
    where [a, b, replaceIndex] = params
    
instruction_changebase :: State -> OpCode -> State
instruction_changebase state@(St ip base input memory output) opcode@(Code code params) = St (ip + 2) (base + baseOffset) input memory output
    where [baseOffset] = params

dispatch :: State -> OpCode -> State
dispatch state opcode@(Code 1 _) = instruction_add state opcode
dispatch state opcode@(Code 2 _) = instruction_mult state opcode
dispatch state opcode@(Code 3 _) = instruction_read state opcode
dispatch state opcode@(Code 4 _) = instruction_write state opcode
dispatch state opcode@(Code 5 _) = instruction_jumpiftrue state opcode
dispatch state opcode@(Code 6 _) = instruction_jumpiffalse state opcode
dispatch state opcode@(Code 7 _) = instruction_lessthan state opcode
dispatch state opcode@(Code 8 _) = instruction_equals state opcode
dispatch state opcode@(Code 9 _) = instruction_changebase state opcode
dispatch state opcode            = error $ "Unknown opcode " ++ (show opcode)

advance :: State -> State
advance state@(St ip base input memory output) = let opcode@(Code code _) = parseOpCode state
    in if code == 99 then state else dispatch state opcode

run :: State -> State
run state@(St ip base input memory output) = let opcode@(Code code _) = parseOpCode state
    in if code == 99 then state else run (dispatch state opcode)
    
runUntilBlocked :: State -> State
runUntilBlocked state@(St ip base input memory output) = if code == 99
    then state
    else (let newState@(St newIp _ _ _ _) = dispatch state opcode
        in if ip == newIp then newState else (runUntilBlocked newState))
    where opcode@(Code code _) = parseOpCode state

    
initState :: [Int] -> [Int] -> State
initState input program = St 0 0 input memory []
    where memory = IntMap.fromList (zip [0..] program)
    
withInput :: State -> [Int] -> State
withInput (St ip base _ memory output) input = (St ip base input memory output)

withOutput :: State -> [Int] -> State
withOutput (St ip base input memory _) output = (St ip base input memory output)

withIO :: State -> [Int] -> [Int] -> State
withIO (St ip base _ memory _) input output = (St ip base input memory output)

alterMemory :: State -> Int -> Int -> State
alterMemory (St ip base input memory output) pos val = (St ip base input (IntMap.insert pos val memory) output)

output :: State -> [Int]
output (St _ _ _ _ output) = output