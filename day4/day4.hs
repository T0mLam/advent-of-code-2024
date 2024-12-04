import Data.List (isPrefixOf, groupBy, sortOn, transpose)

countFreq :: String -> String -> Int
countFreq _ "" = 0
countFreq t s 
    | t `isPrefixOf` s  = 1 + countFreq t (drop (length t) s)
    | otherwise         = countFreq t (tail s)

countTotalXMAS :: String -> [String] -> Int
countTotalXMAS t xs = 
    foldr (\x acc -> countFreq t x + acc) 0 xs +
    foldr (\x acc -> countFreq t (reverse x) + acc) 0 xs

mainDiagonal :: [String] -> [String]
mainDiagonal m = map (map snd) $ (groupBy (\x y -> fst x == fst y) . sortOn fst) diffArray
    where 
        diffArray :: [(Int, Char)]
        diffArray = concat $ zipWith (
            \r ri -> zipWith (\c ci -> (ci - ri, c)) r [0..]  
            ) m [0..] 

antiDiagonal :: [String] -> [String]
antiDiagonal m = map (map snd) $ (groupBy (\x y -> fst x == fst y) . sortOn fst) diffArray
    where 
        diffArray :: [(Int, Char)]
        diffArray = concat $ zipWith (
            \r ri -> zipWith (\c ci -> (ci + ri, c)) r [0..]  
            ) m [0..] 


task1 :: IO ()
task1 = do
    content <- readFile "input.txt"
    let l = lines content
    let total = 
            countTotalXMAS "XMAS" l + 
            countTotalXMAS "XMAS" (transpose l) +
            countTotalXMAS "XMAS" (mainDiagonal l) + 
            countTotalXMAS "XMAS" (antiDiagonal l)
    print total

extract9Grids :: Int -> Int -> [String] -> [String]
extract9Grids r c = map (take 3 . drop c) . (take 3 . drop r)

extractAll9Grids :: [String] -> [[String]]
extractAll9Grids xs = [extract9Grids r c xs | r <- [0 .. length xs - 3], c <- [0 .. length (head xs) - 3]]

task2 :: IO ()
task2 = do
    content <- readFile "input.txt"
    let l = lines content
    let grids = extractAll9Grids l
    let total = length $ filter (
            \g -> 
                countTotalXMAS "MAS" (mainDiagonal g) == 1 &&
                countTotalXMAS "MAS" (antiDiagonal g) == 1
            ) grids
    print total