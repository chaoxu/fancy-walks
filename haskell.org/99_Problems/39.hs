
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `rem` p /= 0]

primesR l r = dropWhile (<l) $ sieve [2..r]
