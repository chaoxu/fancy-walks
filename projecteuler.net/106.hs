
import Data.List

check a b = length a == length b && not (and $ zipWith (<) a b)

solve n = length [(a, b) | a <- subsets, b <- subsets, a /= [] && b /= [] && intersect a b == [] && a < b && check a b]
  where
    subsets = subsequences [1..n]

problem_106 = solve 12

main = print problem_106
