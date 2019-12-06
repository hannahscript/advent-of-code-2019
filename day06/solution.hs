import Common
import Data.List (partition, filter)

type OrbitRelation = (String, String)
data Tree a = Node a [Tree a]

instance Show a => Show (Tree a) where
    show (Node label children) = "(#" ++ (Prelude.show label)++ (Prelude.show children) ++ ")"

parseOrbitRelations :: [String] -> [OrbitRelation]
parseOrbitRelations = map (\l -> parseRelation (splitBy l ')'))
    where parseRelation [center, satellite] = (center, satellite)
    
-- Assume parent exists in tree
insert :: Eq a => a -> a -> Tree a -> Tree a
insert targetParent e (Node parent children) = if targetParent == parent then 
    Node targetParent ((Node e []):children) else
    Node parent (map (insert targetParent e) children)
    
buildTree :: [OrbitRelation] -> [OrbitRelation] -> Tree String -> Tree String
buildTree [] [] tree = tree
buildTree remaining (r:rs) tree = buildTree newRemaining (rs ++ additionalPriority) (insert (fst r) (snd r) tree)
    where (additionalPriority, newRemaining) = partition (\s -> (fst s) == (snd r)) remaining

countOrbits :: Int -> Tree String -> Int
countOrbits depth (Node _ [])  = depth
countOrbits depth (Node _ children) = depth + sum (map (countOrbits (depth + 1)) children)

headOrEmpty :: [[a]] -> [a]
headOrEmpty list = if length list == 0 then [] else head list

search :: String -> [Tree String] -> Tree String -> [Tree String]
search e path node@(Node label []) = if e == label then path else []
search e path node@(Node label children) = if e == label
    then path 
    else headOrEmpty (filter (\p -> length p /= 0) (map (search e (node:path)) children))

dropCommons :: [Tree String] -> [Tree String] -> ([Tree String], [Tree String])
dropCommons [] pathY@(y:ys) = ([], pathY)
dropCommons pathX@(x:xs) [] = (pathX, [])
dropCommons pathX@((Node labelX _):xs) pathY@((Node labelY _):ys) = if labelX == labelY
    then dropCommons xs ys
    else (pathX, pathY)

--- Beep beep lettuce ---

main = do
    content <- getInputAsLines
    let orbits = parseOrbitRelations content
    let tree = buildTree (filter (\r -> (fst r) /= "COM") orbits) (filter (\r -> (fst r) == "COM") orbits) (Node "COM" [])
    print (countOrbits 0 tree)
    let youPath = reverse (search "YOU" [] tree)
    let sanPath = reverse (search "SAN" [] tree)
    let (cutYouPath, cutSanPath) = dropCommons youPath sanPath
    print ((length cutYouPath) + (length cutSanPath))
    
-- Solution 1: 254447
-- Solution 2: 445