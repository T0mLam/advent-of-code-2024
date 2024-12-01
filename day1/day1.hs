import Data.List (sort)

sortTuple :: ([Int], [Int]) -> ([Int], [Int])
sortTuple t = (sort $ fst t, sort $ snd t)

zipDist :: ([Int], [Int]) -> [(Int, Int)]
zipDist = uncurry zip

sumDist :: [(Int, Int)] -> Int
sumDist = foldr (\(x, y) acc -> acc + abs (x - y)) 0  

totalDist :: [(Int, Int)] -> Int
totalDist = sumDist . zipDist . sortTuple . unzip

task1 :: IO ()
task1 = do
    content <- readFile "input.txt"
    let l = lines content
    let ints = map (map read . words) l
    let dist = map (\[x, y] -> (x, y)) ints
    print $ totalDist dist

similarity :: [(Int, Int)] -> Int
similarity zs = foldr f 0 xs
    where
        (xs, ys) = unzip zs
        f :: Int -> Int -> Int
        f x acc = acc + x * length (filter (== x) ys)

task2 :: IO ()
task2 = do
    content <- readFile "input.txt"
    let l = lines content
    let ints = map (map read . words) l
    let dist = map (\[x, y] -> (x, y)) ints
    print $ similarity dist