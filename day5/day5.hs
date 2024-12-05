import Data.Map (Map)
import qualified Data.Map as Map

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of 
    "" -> []
    s' -> let (w, s'') = break p s' in w: wordsWhen p s''

parseRules :: [String] -> [(Int, Int)]
parseRules ("": _) = []
parseRules (x: xs) = (\[x, y] -> (read x, read y)) (wordsWhen (== '|') x): parseRules xs 

constructMap :: [(Int, Int)] -> Map Int [Int]
constructMap = foldr (\(x, y) m -> Map.insertWith (++) x [y] m) Map.empty

parsePages :: [String] -> [[Int]]
parsePages xs = map (map read . wordsWhen (== ',')) $ tail $ dropWhile (/= "") xs 

isInOrder :: Map Int [Int] -> [Int] -> Bool
isInOrder _ [x] = True
isInOrder m (x: y: xs) =
    case Map.lookup x m of 
        Just val -> y `elem` val && isInOrder m (y: xs)
        Nothing -> False
 
task1 :: IO ()
task1 = do 
    content <- readFile "input.txt"
    let l = lines content
    let m = constructMap $ parseRules l
    let pages = parsePages l
    let validPages = filter (isInOrder m) pages
    let sumMiddlePages = sum $ map (\xs -> xs !! (length xs `div` 2)) validPages
    print sumMiddlePages
