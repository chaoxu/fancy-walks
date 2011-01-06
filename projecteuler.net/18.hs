
import IO

grid :: String -> [Integer]
grid = map read . words

mySplit' [] _ = []
mySplit' x n = a:mySplit' b (n+1)
	where (a,b) = splitAt n x

mySplit str = mySplit' str 1

myMerge' last [] = [last]
myMerge' last (x:xs) = (max last x):myMerge' x xs

myMerge :: Integral a => [a] -> [a]
myMerge [] = error "empty list"
myMerge (x:xs) = x:myMerge' x xs

problem_18 input = maximum $ foldl1 (\a b -> zipWith (+) (myMerge a) b) $ mySplit $ grid input

main = do
    file <- openFile "input/p18.txt" ReadMode
    input <- hGetContents file
    print $ problem_18 input
