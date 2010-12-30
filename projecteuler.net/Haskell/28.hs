
lst n = take 4 [n*n, n*n - (n-1)..]

alllst = 1 : concatMap lst [3,5..1001]

problem_28 = sum alllst

