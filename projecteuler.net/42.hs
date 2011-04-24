
import Data.Char

triangleNumbers :: [Int]
triangleNumbers = map (\x -> (x * (x + 1)) `div` 2) [1..32]

isTri :: Int -> Bool
isTri x = x `elem` triangleNumbers

isTriWord :: String -> Bool
isTriWord = isTri . sum . map (\x -> ord x - ord 'A' + 1)

countTriWord :: [String] -> Int
countTriWord = length . filter isTriWord

problem_42 = countTriWord . words . map (\x -> if isLetter x then x else ' ') 

main = readFile "input/words.txt" >>= print . problem_42 
