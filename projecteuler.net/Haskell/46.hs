
import Data.List hiding (union)
import Data.List.Ordered (minus,union)

import Data.Array
 
primes = 2: 3: [5,7..] `minus` foldr union' []
    [ [p*p,p*p+2*p..] | p <- tail primes ]
  where 
    union' (q:qs) xs = q : union qs xs

isPrime = test primes
  where
    test _ n | n <= 1 = False
    test (x:xs) n 
        | x * x > n = True
        | mod n x == 0 = False
        | otherwise = test xs n

check n = not (isPrime n) && [] == takeWhile (>=2) [y | x <- [1..], let y = n - 2 * x * x, y < 2 || isPrime y]

answers = filter check [3,5..]

problem_46 = head answers

--main = print problem_46
