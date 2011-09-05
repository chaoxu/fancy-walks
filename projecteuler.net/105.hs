
import Data.List

isIncreasing :: Ord a => [a] -> Bool
isIncreasing xs = and $ zipWith (<) xs (tail xs)

check seq = isIncreasing $ map snd xs
  where
    xs = sort [(length subseq, sum subseq) | subseq <- subsequences seq]

problem_105 text = sum [sum set | set <- sets, check set]
  where
    sets = [ read $ "[" ++ line ++ "]" | line <- lines text ] :: [[Int]]

main = readFile "input/sets.txt" >>= print . problem_105

