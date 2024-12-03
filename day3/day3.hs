import Text.Regex.TDFA ( (=~), AllTextMatches(getAllTextMatches) )
import Text.Parsec.String (Parser)
import Text.Parsec (many1, string, digit, char, parse)

mulParser :: Parser (Int, Int)
mulParser = do
    _ <- string "mul("
    x <- many1 digit
    _ <- char ','
    y <- many1 digit
    _ <- char ')'
    return (read x, read y)

mulTuples :: [(Int, Int)] -> Int
mulTuples = foldr f 0 
    where
        f :: (Int, Int) -> Int -> Int  
        f (x, y) acc = acc + x * y

task1 :: IO ()
task1 = do
    content <- readFile "input.txt"
    let pattern = "mul\\(([0-9]+),([0-9]+)\\)"
    let matches = getAllTextMatches (content =~ pattern) :: [String]
    let tuples = concatMap (\m -> case parse mulParser "" m of 
            Left _ -> []
            Right r -> [r]) matches 
    print $ mulTuples tuples 

removeMul :: Int -> [String] -> [String]
removeMul _ [] = []
removeMul n (x: xs)
    | x == "don't()"    = removeMul 0 xs 
    | x == "do()"       = removeMul 1 xs 
    | n == 1            = x: removeMul n xs
    | otherwise         = removeMul n xs

task2 :: IO ()
task2 = do
    content <- readFile "input.txt"
    let pattern = "do(n't)?\\(\\)|mul\\(([0-9]+),([0-9]+)\\)"
    let matches = getAllTextMatches (content =~ pattern) :: [String]
    let removedMatches = removeMul 1 matches
    let tuples = concatMap (\m -> case parse mulParser "" m of 
            Left _ -> []
            Right r -> [r]) removedMatches
    print $ mulTuples tuples 