
fact 0 = 1
fact n = n * fact (n - 1)

comb n m = fact n `div` (fact m * fact (n - m))

problem_53 = length [(n,m) | n <- [1..100], m <- [0..n], comb n m > 1000000]

main = print problem_53
