
fib = 1 : 1 : zipWith (+) fib (tail fib)

problem_2 = sum $ filter even $ takeWhile (<=4000000) fib

main = print problem_2
