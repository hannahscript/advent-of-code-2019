import Common

import qualified Data.Map.Strict as Map

import Debug.Trace

-- Name, Amount
data Chemical = Chemical String Int deriving Show

multiplyChemical :: Int -> Chemical -> Chemical
multiplyChemical factor (Chemical name amount) = Chemical name (factor * amount)

-- Product, Reagents
data Reaction = Reaction Chemical [Chemical] deriving Show

multiplyReaction :: Int -> Reaction -> Reaction
multiplyReaction factor (Reaction product reagents) = Reaction (multiplyChemical factor product) (map (multiplyChemical factor) reagents)

type ReactionMap = Map.Map String Reaction

-- Crappy parser
stripLeft :: String -> String
stripLeft = dropWhile (== ' ')

parseChemical :: String -> Chemical
parseChemical str = Chemical name (stringToInt amount)
    where [amount, name] = splitBy str ' '

parseReagents :: String -> [Chemical]
parseReagents str = map (parseChemical . stripLeft) (splitBy (takeWhile (/= '=') str) ',')

parseProduct :: String -> Chemical
parseProduct str = parseChemical $ stripLeft $ drop 1 $ dropWhile (/= '>') str

parseReaction :: String -> Reaction
parseReaction str = Reaction chemical reagents
    where chemical = parseProduct str
          reagents = parseReagents str

parseReactions :: [String] -> ReactionMap
parseReactions reactionStrings = Map.fromList $ map (\reaction -> (productName reaction, reaction)) reactions
    where reactions = map parseReaction reactionStrings
          productName (Reaction (Chemical name _) _) = name

-- Part 1

type Leftovers = Map.Map String Int

produce :: ReactionMap -> Leftovers -> Chemical -> (Leftovers, Int)
produce reactionMap leftovers target@(Chemical name wantedAmount)
    | name == "ORE"                 = (leftovers, wantedAmount)
    | chemLeftovers >= wantedAmount = (Map.insert name (chemLeftovers - wantedAmount) leftovers, 0)
    | otherwise                     = (Map.insert name productLeftovers subreactionLeftovers, reactionOre)
    where
        chemLeftovers                                        = Map.findWithDefault 0 name leftovers
        reaction                                             = reactionMap Map.! name
        factor                                               = reactionFactor reaction target chemLeftovers
        (Reaction (Chemical _ actualProductAmount) reagents) = multiplyReaction factor reaction
        (subreactionLeftovers, reactionOre)                  = foldl (\(left, ore) reagent -> let (newLeft, newOre) = produce reactionMap left reagent in (newLeft, ore + newOre)) (leftovers, 0) reagents
        productLeftovers                                     = actualProductAmount + chemLeftovers - wantedAmount
        
reactionFactor :: Reaction -> Chemical -> Int -> Int
reactionFactor (Reaction (Chemical _ productAmount) _) (Chemical _ wantedAmount) leftoverAmount
    | leftoverAmount >= wantedAmount = 0
    | otherwise                      = ceiling ( (fromIntegral (wantedAmount - leftoverAmount)) / (fromIntegral productAmount) )


-- Part 2
produce' :: ReactionMap -> Chemical -> Int
produce' reactionMap product = ore
    where (_, ore) = produce reactionMap Map.empty product

trillion = 1000000000000 :: Int

findFirst :: (Int -> a) -> (a -> Bool) -> Int -> Int -> (Int, a)
findFirst producer predicate start step = head results
    where searchSpace = map (\n -> (n, producer n)) [start,start+step..]
          results     = dropWhile (not . predicate . snd) searchSpace
          
steppingFindFirst :: (Int -> a) -> (a -> Bool) -> Int -> Int -> (Int, a)
steppingFindFirst producer predicate start 1    = findFirst producer predicate start 1
steppingFindFirst producer predicate start step = steppingFindFirst producer predicate (overshoot - step) (step `div` 2)
    where (overshoot, _) = findFirst producer predicate start step

main = do
    content <- getInputAsLines
    let reactionMap = parseReactions content
    let (_, ore) = produce reactionMap Map.empty (Chemical "FUEL" 1)
    putStrLn ("Ore needed: " ++ show ore)
    let searchStart = trillion `div` ore
    let (fuelPlusOne, _) = steppingFindFirst (\n -> produce' reactionMap (Chemical "FUEL" n)) (> trillion) searchStart 10000
    putStrLn ("Fuel for 1e12 ore: " ++ show (fuelPlusOne - 1))
    
-- Solution 1: 261960
-- Solution 2: 4366186