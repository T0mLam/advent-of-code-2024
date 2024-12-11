import Data.Char (digitToInt)

buildFileBlocks :: Int -> Int -> String -> [String]
buildFileBlocks _ _ [] = []
buildFileBlocks n c (x: xs) 
    | even n    = replicate (digitToInt x) (show c) ++ buildFileBlocks (n + 1) (c + 1) xs
    | otherwise = replicate (digitToInt x) "." ++ buildFileBlocks (n + 1) c xs

shiftDigits :: [String] -> [String] 
shiftDigits xs = go 0 (length xs - 1) xs
    where 
        go :: Int -> Int -> [String] -> [String]
        go l r xs 
            | l >= r            = xs
            | xs !! l /= "."    = go (l + 1) r xs
            | xs !! l == "."    = go l (r - 1) xs
            | otherwise         = go (l + 1) (r - 1) (swap l r xs)
        swap :: Int -> Int -> [String] -> [String]
        swap l r xs = take l xs ++ [xs !! r] ++ drop l (take r xs) ++ [xs !! l] ++ drop (r + 1) xs


-- Unfinished
task1 :: IO ()
task1 = do 
    content <- readFile "eg.txt"
    let b = buildFileBlocks 0 0 content 
    print $ shiftDigits b
