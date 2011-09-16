
-- R(10^n) = (10^(10^n) - 1) / 9
-- R(10^n) % p == 0  <==>  10^(10^n) % (9 * n) == 1
--                   <==>  10^(10^n) % 9 == 1(surely) && 10^(10^n) % p == 1
--                   <==>  10^(10^n % (p - 1)) % p == 1
--                   <==>  10^n % (p - 1) should be mutiple of order(10, p)
--                   <==>  order (10, p) contains only 2^x*5^y
--                   <==>  order (10, p) divides phi(p) = p - 1
--                   <==>  enumerate all 2^x * 5 ^ y factors of p - 1

import Math.Sieve.ONeill
import Data.Int
import Data.List

powMod a 0 m = 1
powMod a p m | odd p     = powMod a (p - 1) m * a `mod` m
             | otherwise = let b = powMod a (p `div` 2) m in b * b `mod` m

check 3 = False
check p = not (null lst)
  where
    go n x | n `mod` x == 0 = x : go (n `div` x) x
           | otherwise      = []
    
    factor2 = scanl (*) 1 $ go (p - 1) 2
    factor5 = scanl (*) 1 $ go (p - 1) 5

    lst = [ f2 * f5
          | f2 <- factor2
          , f5 <- factor5
          , powMod 10 (f2 * f5) p == 1
          ]

problem_133 = sum [p | p <- takeWhile (<100000) primes, not (check p)]

main = print problem_133
