import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.Vty (Cursor(PositionOnly))

type Position = (Int, Int)

constructMap :: [String] -> Map Char [Position]
constructMap xs = foldr f Map.empty listWithIndex
    where 
        listWithIndex :: [(Position, Char)]
        listWithIndex = [((r, c), x) | (r, row) <- zip [0..] xs, (c, x) <- zip [0..] row, x `notElem` ['.', '#']]
        f :: (Position, Char) -> Map Char [Position] -> Map Char [Position]
        f ((r, c), x) = Map.insertWith (++) x [(r, c)]

getAntinodeMap :: Int -> Int -> [Position] -> Map Position Int
getAntinodeMap maxR maxC xs = Map.unions [
    Map.fromList $ getAntinodeList (xs !! i) $ take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]
    ]
    where 
        getAntinodeList :: Position -> [Position] -> [(Position, Int)]
        getAntinodeList (tr, tc) = foldr f []
            where 
                diff :: Position -> Position -> Position
                diff (r1, c1) (r2, c2) = (r1 - r2, c1 - c2)
                f :: Position -> [(Position, Int)] -> [(Position, Int)] 
                f (r, c) acc = 
                    if tr + dr `elem` [0 .. maxR - 1] && tc + dc `elem` [0 .. maxC - 1]
                        then ((tr + dr, tc + dc), 1): acc
                        else acc
                    where 
                        (dr, dc) = diff (tr, tc) (r, c)

task1 :: IO ()
task1 = do
    content <- readFile "input.txt"
    let l = lines content 
    let m = constructMap l
    let (maxR, maxC) = (length l, length (l !! 1))
    let antiNodeMap = foldr (\x acc -> Map.union acc (getAntinodeMap maxR maxC x)) Map.empty (Map.elems m)
    print $ Map.size antiNodeMap

getAntinodeMap' :: Int -> Int -> [Position] -> Map Position Int
getAntinodeMap' maxR maxC xs = Map.unions [
    Map.fromList $ getAntinodeList (xs !! i) $ take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]
    ]
    where 
        getAntinodeList :: Position -> [Position] -> [(Position, Int)]
        getAntinodeList (tr, tc) = foldr f []
            where 
                diff :: Position -> Position -> Position
                diff (r1, c1) (r2, c2) = (r1 - r2, c1 - c2)
                f :: Position -> [(Position, Int)] -> [(Position, Int)] 
                f (r, c) acc = go (tr, tc) acc
                    where 
                        (dr, dc) = diff (tr, tc) (r, c)
                        go :: Position -> [(Position, Int)] -> [(Position, Int)] 
                        go (nr, nc) acc
                            | nr `elem` [0 .. maxR - 1] && nc `elem` [0 .. maxC - 1] = 
                                go (nr + dr, nc + dc) (((nr, nc), 1): acc)
                            | otherwise = acc

task2 :: IO ()
task2 = do
    content <- readFile "input.txt"
    let l = lines content 
    let m = constructMap l
    let (maxR, maxC) = (length l, length (l !! 1))
    let antiNodeMap = foldr (\x acc -> Map.union acc (getAntinodeMap' maxR maxC x)) Map.empty (Map.elems m)
    print $ Map.size antiNodeMap