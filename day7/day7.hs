removeColon :: String -> String
removeColon s = 
    case s !! (length s - 1) of 
        ':' -> init s
        _ -> s

possibleResults :: [Int] -> [Int]
possibleResults [] = []
possibleResults [x] = [x]
possibleResults (x: xs) = concatMap (\y -> [x + y, x * y]) remain
    where 
        remain = possibleResults xs

isValidComb :: ([Int] -> [Int]) -> (Int, [Int]) -> Bool
isValidComb f (target, xs) = target `elem` f (reverse xs)

task1 :: IO ()
task1 = do
    content <- readFile "input.txt"
    let l = map words $ lines content
    let n = map (map ((read :: String -> Int) . removeColon)) l
    let op = map (\(x: xs) -> (x, xs)) n
    let filteredOp = filter (isValidComb possibleResults) op
    let total = foldr (\t acc -> fst t + acc) 0 filteredOp
    print total

possibleResults' :: [Int] -> [Int]
possibleResults' [] = []
possibleResults' [x] = [x]
possibleResults' (x: xs) = concatMap (\y -> 
    [x + y, x * y, read (show y ++ show x)]
    ) remain
    where 
        remain = possibleResults' xs

task2 :: IO ()
task2 = do
    content <- readFile "input.txt"
    let l = map words $ lines content
    let n = map (map ((read :: String -> Int) . removeColon)) l
    let op = map (\(x: xs) -> (x, xs)) n
    let filteredOp = filter (isValidComb possibleResults') op
    let total = foldr (\t acc -> fst t + acc) 0 filteredOp
    print total