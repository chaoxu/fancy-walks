
import Data.Char
import Data.List

solve names = sum $ zipWith score [1..] (sort names)	
  where
	score ind name = (*ind) . sum . map (\c -> ord (toLower c) - ord 'a' + 1) $ name

problem_22 = solve . words . map (\x -> if isLetter x then x else ' ')

main = readFile "input/names.txt" >>= print . problem_22 
