
isPrime :: Integer -> Bool

isPrime = isPrime2 2 where 
    isPrime2 k n | k * k > n = True
    isPrime2 k n | n `rem` k == 0 = False
    isPrime2 k n = isPrime2 (k+1) n
