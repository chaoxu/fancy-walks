
problem_5 = foldl (\x y -> x * y `div` (gcd x y)) 1 [2..20]

main = print problem_5
