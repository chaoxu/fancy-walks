import Data.List hiding (union)
import Data.List.Ordered (minus,union)
import Data.Int

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

list n = [n * n - (n-1) * k | k <- [0..3]]

solve c d (x:xs) 
    | (d+dd) * 10 < c+dc = x
    | otherwise = solve (c+dc) (d+dd) xs
  where
    lst = list x
    dc = length lst
    dd = length $ filter isPrime lst

problem_58 = solve 1 0 [3,5..]

main = print problem_58
