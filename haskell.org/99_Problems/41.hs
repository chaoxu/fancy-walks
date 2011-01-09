
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `rem` p /= 0]

primes = sieve [2..]

goldbach n
        | n < 4 || n `mod` 2 == 1 = error "invalid input"
        | otherwise               = helper primesInRange (reverse primesInRange)
        where 
            primesInRange = takeWhile (<= n) primes
            helper _ [] = error " you find a counter-example"
            helper [] _ = error " you find a counter-example"
            helper (x:xs) (y:ys) 
                | x + y < n  = helper xs (y:ys)
                | x + y > n  = helper (x:xs) ys
                | otherwise  = (x,y)

goldbachList lower upper
    | lower > upper      = []
    | lower `mod` 2 == 1 = goldbachList (lower+1) upper
    | lower < 4          = goldbachList 4 upper
    | otherwise          = (goldbach lower) : goldbachList (lower+1) upper

