
-- (x + k)^2 - x^2 - (x - k)^2 = n
-- 4xk - x^2 = n
-- x^2 - 4xk + n = 0 && x > k > 0
-- x * (x - 4k) + n = 0
-- x * (4k - x) = n
-- 
-- n % x == 0
-- (n / x + x) % 4 == 0
-- 4 * x > n / x + x ==> 3 * x > n / x == > 3 * x^2 > n  ==> x > sqrt (n / 3)
--
-- n % 4 == 2   ==>  No Solution
-- n % 4 == 1   ==>  No Solution
-- n % 4 == 3   ==>  1 * 3 || 3 * 1, 
--                   all divisors s.t. (n / x + x) % 4 == 0, 
--                   if n is not a prime, have at least two divisors greater than sqrt n.
--                   so n must be a prime
-- n % 4 == 0   ==>  0 * 0 || 2 * 2
--                   first consider 2 * 2, n = 4 * (2m + 1), 2m + 1 must be a prime, 
--                   now consider 0 * 0, n = 4 * 4 * m, m must be a prime

import Control.Monad
import Control.Monad.State
import System.Random

import qualified Data.Numbers.Primes as P

checkIsPrime n = state $ P.isProbablyPrime n

isPrime 1 = True
--isPrime n = all (\x -> n `mod` x /= 0) $ takeWhile (\x -> x * x <= n) primes

isPrime n = and $ evalState (replicateM 20 (checkIsPrime n)) (mkStdGen 3137)

check n = case n `mod` 4 of
    1 -> False
    2 -> False
    3 -> isPrime n
    0 -> let m = n `div` 4 in if odd m then isPrime m else m >= 4 && m `mod` 8 == 4 && isPrime (m `div` 4)


limit = 50*10^6

problem_136 = length [n | n <- [1..limit-1], check n]

main = print problem_136
