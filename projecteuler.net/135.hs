
-- (x + k)^2 - x^2 - (x - k)^2 = n
-- 4xk - x^2 = n
-- x^2 - 4xk + n = 0 && x > k > 0
-- x * (x - 4k) + n = 0
-- x * (4k - x) = n

import Math.Sieve.Factor
import Test.QuickCheck

limit = 10^6

fs = sieve limit

divisors n = map product $ mapM (\(a, b) -> map (a^) [0..b]) $ factor fs n

prop_divisors (Positive n) = n > limit || and [n `mod` d == 0 | d <- divisors n]

solve n = [ (x + k, x, x - k)
          | x <- divisors n
          , let fourk = n `div` x + x
          , fourk `mod` 4 == 0
          , let k = fourk `div` 4
          , x > k && k > 0
          ]

problem_135 = length [n | n <- [1..limit-1], length (solve n) == 10]

main = print problem_135
