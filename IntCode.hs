module IntCode (
    State (St),
    parseProgram,
    initState,
    run,
    advance
) where

import Common
import Data.Char (digitToInt)
import qualified Data.Map as Map
import Debug.Trace

-- Instruction pointer, relativeBase, Input, Memory, Output
data State = St Int Int [Int] (Map.Map Int Int) [Int]

-- Actual opcode, parameters
data OpCode = Code Int [Int]

data ParameterMode = Dest | Src deriving Show
type Parameter = (Int, ParameterMode)

instance Show OpCode where
    show (Code code params) = "#" ++ (Prelude.show code) ++ " [" ++ (Prelude.show params) ++ "]"
    
instance Show State where
    show (St ip _ input _ output) = "IP#" ++ (Prelude.show ip) ++ " IN=" ++ (Prelude.show input) ++ " OUT=" ++ (Prelude.show output)

(!) :: (Map.Map Int Int) -> Int -> Int
map ! i = Map.findWithDefault 0 i map

getParameterTypes :: Int -> [ParameterMode]
getParameterTypes 1 = [Src, Src, Dest] -- add
getParameterTypes 2 = [Src, Src, Dest] -- mult
getParameterTypes 3 = [Dest]       -- read
getParameterTypes 4 = [Src]       -- write
getParameterTypes 5 = [Src, Src]    -- jumpiftrue
getParameterTypes 6 = [Src, Src]    -- jumpiffalse
getParameterTypes 7 = [Src, Src, Dest] -- lessthan
getParameterTypes 8 = [Src, Src, Dest] -- equals
getParameterTypes 9 = [Src]       -- changebase
getParameterTypes 99 = []       -- end
    
addDefaults :: [Int] -> [Int] -> [Int]
addDefaults defaultList list = list ++ defaultTail
    where defaultTail = drop (length list) defaultList
    
getParameters :: Int -> [Int] -> [Parameter]
getParameters code paramModes = zip (paramModes ++ paramTail) types
    where types = getParameterTypes code
          paramTail = take (length types - length paramModes) $ repeat 0

getParameterValues :: State -> [Parameter] -> [Int]
getParameterValues (St ip base input memory output) paramModes = mapWithIndex (\(mode, ptype) offset -> getParam (offset + 1) mode ptype) paramModes
    where immediateValue offset = memory ! (ip + offset)
          getParam offset 0 Dest     = immediateValue offset                   -- position mode, dest
          getParam offset 0 Src     = memory ! (immediateValue offset)         -- position mode, src
          getParam offset 1 _     = immediateValue offset                      -- immediate mode, src (dest will never be immediate)
          getParam offset 2 Dest    = base + (immediateValue offset)           -- relative mode, dest
          getParam offset 2 Src    = memory ! (base + (immediateValue offset)) -- relative mode

parseOpCode :: State -> OpCode
parseOpCode state@(St ip base input memory output) = Code code params
    where num = memory ! ip
          (codeR : codeL : paramModes) = reverse (let n = show num in if length n == 1 then '0':n else n)
          code = stringToInt (codeL:codeR:[])
          params = getParameterValues state (getParameters code (map digitToInt paramModes))
          
parseProgram :: [String] -> [Int]
parseProgram program = map stringToInt program

instruction_add :: State -> OpCode -> State
instruction_add state@(St ip base input memory output) opcode@(Code code params) = St (ip + 4) base input (Map.insert replaceIndex (a + b) memory) output
    where [a, b, replaceIndex] = params
          
instruction_mult :: State -> OpCode -> State
instruction_mult state@(St ip base input memory output) opcode@(Code code params) = St (ip + 4) base input (Map.insert replaceIndex (a * b) memory) output
    where [a, b, replaceIndex] = params
          
instruction_read :: State -> OpCode -> State
instruction_read state@(St ip base [] memory output) opcode@(Code code params) = state -- If input is empty, wait
instruction_read state@(St ip base input memory output) opcode@(Code code params) = St (ip + 2) base restInput (Map.insert replaceIndex i memory) output
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
instruction_lessthan state@(St ip base input memory output) opcode@(Code code params) = St (ip + 4) base input (Map.insert replaceIndex (if a < b then 1 else 0) memory) output
    where [a, b, replaceIndex] = params
    
instruction_equals :: State -> OpCode -> State
instruction_equals state@(St ip base input memory output) opcode@(Code code params) = St (ip + 4) base input (Map.insert replaceIndex (if a == b then 1 else 0) memory) output
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

advance :: State -> State
advance state@(St ip base input memory output) = let opcode@(Code code _) = parseOpCode state
    in if code == 99 then state else dispatch state opcode

run :: State -> State
run state@(St ip base input memory output) = let opcode@(Code code _) = parseOpCode state
    in if code == 99 then state else run (dispatch state opcode)
    

    
initState :: [Int] -> [Int] -> State
initState input program = St 0 0 input memory []
    where memory = Map.fromList (zip [0..] program)