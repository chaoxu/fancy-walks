
combine n m = product [n - m + 1..n] `div` product [1..m]

increasingNumber digits len = combine (len + digits - 1) len

solve len = increasingNumber 10 len + increasingNumber 9 len - 10

problem_113 = sum $ map solve [1..100]

main = print problem_113
