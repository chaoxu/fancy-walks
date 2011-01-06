
import Data.Char
import IO

largest :: [Char] -> Int
largest (a:(xs@(b:c:d:e:_))) = max (product $ map digitToInt (a:b:c:d:e:[])) $ largest xs
largest _ = 0

problem_8 input = largest input

main = do
    file <- openFile "input/p8.txt" ReadMode
    input <- fmap (filter isDigit) $ hGetContents file
    print $ problem_8 input
