
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `rem` p /= 0]

goldbach n  
        | n < 4 || n `mod` 2 == 1 = error "invalid input"
        | otherwise               = helper primes (reverse primes)
        where 
            primes = sieve [2..n]
            helper _ [] = error " you find a counter-example"
            helper [] _ = error " you find a counter-example"
            helper (x:xs) (y:ys) 
                | x + y < n  = helper xs (y:ys)
                | x + y > n  = helper (x:xs) ys
                | otherwise  = (x,y)

