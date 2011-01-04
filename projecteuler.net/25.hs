
fib = 1 : 1 : (map (uncurry (+)) $ zip fib (tail fib))

problem_25 = (+1).length $ takeWhile ((<1000).length.show) fib
