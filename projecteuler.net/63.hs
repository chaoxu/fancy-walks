
import Data.List

list :: [Integer]
list = nub $ sort [nk | k <- [1..1000], n <- [1..9], let nk = n^k, length (show nk) == k]

problem_63 = length list

main = print problem_63
