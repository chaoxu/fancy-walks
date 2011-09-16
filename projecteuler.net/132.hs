
import Math.Sieve.ONeill

powmod a 0 m = 1 `mod` m
powmod a b m | even b    = let c = powmod a (b `div` 2) m in c^2 `mod` m
             | otherwise = powmod a (b-1) m * a `mod` m

prop_powmod n = n < 0 || n > 97 || powmod 2 n 97 == (2^n) `mod` 97

checkDivide ones n = (powmod 10 ones (n * 9) - 1) `mod` (n * 9) `div` 9 == 0

problem_132 = sum $ take 40 [p | p <- primes, checkDivide (10^9) p]

main = print problem_132
