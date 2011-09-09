
f m = lst
  where
    lst = replicate m 1 ++ zipWith (+) lst (drop (m-1) lst)

f2 n = f 2 !! n - 1
f3 n = f 3 !! n - 1
f4 n = f 4 !! n - 1

solve n = f2 n + f3 n + f4 n

problem_116 = solve 50

main = print problem_116
