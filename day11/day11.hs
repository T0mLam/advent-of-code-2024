blink :: [String] -> [String]
blink [] = []
blink (x: xs) 
    | x == "0"          = "1": blink xs
    | even (length x)   = first: second: blink xs 
    | otherwise         = show (2024 * read x): blink xs
    where 
        newStones :: (String, String)
        newStones = splitAt (length x `div` 2) x
        first :: String
        first = fst newStones
        second :: String 
        second = show $ (read :: String -> Int) $ snd newStones

blinkN :: Int -> [String] -> [String]
blinkN n xs = foldr (\f acc -> f acc) xs (replicate n blink)

task1 :: IO ()
task1 = do
    content <- readFile "input.txt"
    let l = words content
    let stones = blinkN 25 l
    print $ length stones