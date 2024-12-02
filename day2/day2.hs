allIncreasing :: [Int] -> Bool
allIncreasing [x] = True
allIncreasing (x: xs) = (x < y) && (y - x `elem` [1..3]) && allIncreasing xs
    where 
        y = head xs

allDecreasing :: [Int] -> Bool
allDecreasing [x] = True
allDecreasing (x: xs) = (x > y) && (x - y `elem` [1..3]) && allDecreasing xs
    where 
        y = head xs

isSafe :: [Int] -> Bool
isSafe xs = allIncreasing xs || allDecreasing xs

task1 :: IO ()
task1 = do 
    content <- readFile "input.txt"
    let l = map (map read . words) $ lines content
    print $ length $ filter isSafe l

anySafe :: [Int] -> Bool
anySafe xs = or [isSafe $ removeAt i xs | i <- [0 .. length xs - 1]]
    where 
        removeAt :: Int -> [Int] -> [Int]
        removeAt i xs = take i xs ++ drop (i + 1) xs

task2 :: IO ()
task2 = do 
    content <- readFile "input.txt"
    let l = map (map read . words) $ lines content
    print $ length $ filter anySafe l