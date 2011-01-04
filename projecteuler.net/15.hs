

f :: Integer -> Integer

f 0 = 1
f n = f (n-1) * n

comb a b = (f a) `div` (f (a-b) * f b)

problem_15 = comb 40 20
