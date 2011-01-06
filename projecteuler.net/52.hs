
import Data.List

check a b = sort (show a) == sort (show b)

checkN n = all (check n) [2*n,3*n..6*n]

problem_52 = head $ filter checkN [1..]

main = print problem_52
