
import Data.Char
import Data.List
import IO

problem_22 names = sum $ zipWith score [1..] (sort names)	where
	score ind name = (*ind) . sum . map (\c -> ord (toLower c) - ord 'a' + 1) $ name

main = do 
    file <- openFile "input/names.txt" ReadMode
    names <- fmap (words.map (\x -> if isLetter x then x else ' ')) $ hGetContents file
    print $ problem_22 names
