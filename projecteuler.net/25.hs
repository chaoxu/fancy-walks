
fib = 1 : 1 : zipWith (+) fib (tail fib)

problem_25 = (+1).length $ takeWhile ((<1000).length.show) fib

main = print problem_25
